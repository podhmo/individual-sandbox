import { parseArgs } from "jsr:@podhmo/with-help@0.5.2";
import { withTrace } from "jsr:@podhmo/build-fetch@0.1.0";
import "jsr:@std/dotenv/load";

// TODO: bearer token のキャッシュを実装する
// TODO: スレッドの投稿に対応する

// see: https://docs.bsky.app/docs/api/com-atproto-repo-create-record

// deno run -A main.ts --content "hello, world" --debug

async function main() {
    const args = parseArgs(Deno.args, {
        string: [
            "identifier",
            "password",
            "content",
        ],
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

    const post = async (content: string) => {
        await Bluesky.post({ ...args, content });
    };

    try {
        if (args.content !== undefined) {
            let content = args.content;
            if (content.startsWith("file:")) {
                const path = content.slice("file:".length);
                content = await Deno.readTextFile(path);
            }
            await post(content);
        } else {
            const decoder = new TextDecoder();

            for await (const chunk of Deno.stdin.readable) {
                const content = decoder.decode(chunk);
                await post(content);
            }
        }
    } catch (error) {
        console.error(error);
    }
}

// deno-lint-ignore no-namespace
namespace Bluesky {
    export async function post(options: {
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
            throw new Error(
                `Failed to authenticate: ${authResponse.statusText}`,
            );
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
            record: { // https://github.com/bluesky-social/atproto/blob/main/lexicons/app/bsky/feed/post.json#L9
                $type: "app.bsky.feed.post",
                text: content,
                createdAt,
                reply: { // https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/repo/strongRef.json
                    root: {
                        uri: "at://did:plc:hpog7qvhzybjzzjq3p5eq6ei/app.bsky.feed.post/3lcv345ggho2w",
                        cid: "bafyreidydm6jeszhue7wqemkbsb4gkqm2mk6kcgy35o4blt6mptnm6mnsu",
                    },
                    parent: {
                        uri: "at://did:plc:hpog7qvhzybjzzjq3p5eq6ei/app.bsky.feed.post/3lcv46oyhud23",
                        cid: "bafyreiegwbry7d63gxcp4uepws4lx3sqknzf3d4kmgfdo6xotkw4qfzfya",
                    },
                },
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
}

if (import.meta.main) {
    await main();
}
