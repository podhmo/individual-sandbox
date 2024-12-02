import { parseArgs } from "jsr:@podhmo/with-help@0.5.2";
import "jsr:@std/dotenv/load";

async function main() {
    const args = parseArgs(Deno.args, {
        string: ["identifier", "password", "content"],
        boolean: ["debug"],
        required: ["identifier", "password"],
        envvar: {
            identifier: "BSKY_IDENTIFIER",
            password: "BSKY_PASSWORD",
            debug: "DEBUG",
        },
        flagDescription: {
            content: "content of the post. if not provided, read from stdin",
        },
    });

    const decoder = new TextDecoder();
    try {
        if (args.content !== undefined) {
            let content = args.content;
            if (content.startsWith("file:")) {
                const path = content.slice("file:".length);
                content = await Deno.readTextFile(path);
            }
            await postToBluesky({ ...args, content });
        } else {
            for await (const chunk of Deno.stdin.readable) {
                const content = decoder.decode(chunk);
                await postToBluesky({ ...args, content });
            }
        }
    } catch (error) {
        console.error(error);
    }
}

async function postToBluesky(options: {
    identifier: string;
    password: string;
    content: string;
    debug: boolean;
}) {
    const { identifier, password, content } = options;

    let fetch = globalThis.fetch;
    if (options.debug) {
        fetch = withTrace(fetch);
    }

    const baseUrl = "https://bsky.social/xrpc";

    // 認証エンドポイント
    const authEndpoint = `${baseUrl}/com.atproto.server.createSession`;

    // 認証リクエスト
    const authResponse = await fetch(authEndpoint, {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
        },
        body: JSON.stringify({
            identifier: identifier,
            password: password,
        }),
    });

    if (!authResponse.ok) {
        throw new Error(`Failed to authenticate: ${authResponse.statusText}`);
    }

    const authData = await authResponse.json();
    const accessJwt = authData.accessJwt;

    // 投稿エンドポイント
    const postEndpoint = `${baseUrl}/com.atproto.repo.createRecord`;

    // 現在時刻をISO 8601形式で取得
    const createdAt = new Date().toISOString();

    // 投稿データ
    const postData = {
        repo: authData.did, // ユーザーのDID
        collection: "app.bsky.feed.post",
        record: {
            $type: "app.bsky.feed.post",
            text: content,
            createdAt,
        },
    };

    // 投稿リクエスト
    const postResponse = await fetch(postEndpoint, {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
            Authorization: `Bearer ${accessJwt}`,
        },
        body: JSON.stringify(postData),
    });

    if (!postResponse.ok) {
        throw new Error(`Failed to post: ${postResponse.statusText}`);
    }

    const postResult = await postResponse.json();
    console.log("Post successful:", postResult);
}

export function withTrace(inner: typeof globalThis.fetch) {
    return async function fetchWithTrace(
        url: Parameters<typeof globalThis.fetch>[0],
        init?: Parameters<typeof globalThis.fetch>[1],
    ) {
        const headers = init?.headers as Record<string, string> ?? {};

        // trace request
        console.error({
            request: { url, method: init?.method, headers, body: init?.body },
        });

        const response = await inner(url, { ...init, headers });

        // trace response
        if (!response.ok) {
            console.error({ response, text: await response.text() });
            throw new Error(
                `Error: ${response.status} - ${response.statusText}`,
            ); // ここで例外を投げないとresponseを後続で消費されてしまう
        }
        console.error({ response }); // 正常系のときにレスポンスを消費したくない
        return response;
    };
}

if (import.meta.main) {
    await main();
}
