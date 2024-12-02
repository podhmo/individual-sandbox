import { parseArgs } from "jsr:@podhmo/with-help@0.5.2";
import "jsr:@std/dotenv/load";

async function main() {
    const args = parseArgs(Deno.args, {
        string: [
            "identifier",
            "password",
            "content",
            "gemini-apikey",
            "gemini-model",
        ],
        boolean: ["debug"],
        required: ["identifier", "password", "gemini-apikey", "gemini-model"],
        envvar: {
            identifier: "BSKY_IDENTIFIER",
            password: "BSKY_PASSWORD",
            debug: "DEBUG",
            "gemini-apikey": "GEMINI_API_KEY",
            "gemini-model": "GEMINI_MODEL",
        },
        default: {
            "gemini-model": Gemini.BASE_MODEL,
        },
        flagDescription: {
            content: "content of the post. if not provided, read from stdin",
        },
    });

    // geminiで翻訳してからblueskyに投稿する
    const post = async (content: string) => {
        console.dir({ ...args, content });
        const fetch = Gemini.buildFetch(globalThis.fetch, {
            apiKey: args["gemini-apikey"],
            model: args["gemini-model"] as Gemini.Model,
            debug: args.debug,
        });

        let generatedText = "";
        {
            const text = content;
            const prompt =
                `以下の文章を英語に翻訳してください。\n-----\n${text}`;
            const payload = { "contents": [{ "parts": { "text": prompt } }] };
            const response = await fetch(
                "/v1beta/{model=models/*}:generateContent",
                {
                    method: "POST",
                    body: JSON.stringify(payload),
                },
            );
            const data = await response.json();
            generatedText = data.candidates[0].content.parts[0].text;
        }

        // console.log(JSON.stringify(data, null, 2));
        console.log("input: ", content);
        // console.dir(data, { depth: 10 });
        console.log("output: ", generatedText);

        let revivedText = "";
        {
            const text = generatedText;
            const prompt =
                `以下の英語の文章を日本語に翻訳してください。\n-----\n${text}`;
            const payload = { "contents": [{ "parts": { "text": prompt } }] };
            const response = await fetch(
                "/v1beta/{model=models/*}:generateContent",
                {
                    method: "POST",
                    body: JSON.stringify(payload),
                },
            );
            const data = await response.json();
            revivedText = data.candidates[0].content.parts[0].text;
        }
        console.log("revived: ", revivedText);

        const postContent =
            `元の文章: ${content.trimEnd()}\n\n翻訳: ${generatedText.trimEnd()}\n\n再翻訳: ${revivedText.trimEnd()}`;

        await Bluesky.post({ ...args, content: postContent });
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
}

// deno-lint-ignore no-namespace
namespace Gemini {
    export const BASE_URL = "https://generativelanguage.googleapis.com";
    export const BASE_MODEL: Model = "gemini-1.5-flash-8b";

    export interface APIDoc {
        "/v1beta/{model=models/*}:generateContent":
            "https://ai.google.dev/gemini-api/docs/text-generation";
        "/v1beta/{model=models/*}:streamGenerateContent":
            "https://ai.google.dev/api/generate-content?hl=ja#v1beta.models.streamGenerateContent";
    }

    // model https://ai.google.dev/gemini-api/docs/models/gemini?hl=ja
    export const models = [
        "gemini-1.5-flash",
        "gemini-1.5-flash-8b",
        "gemini-1.5-pro",
    ] as const;
    export type Model = typeof models[number];

    export type Endpoint = keyof APIDoc;

    /** the specialized version of fetch() function for Gemini */
    export function buildFetch(
        inner: typeof globalThis.fetch,
        options: {
            apiKey: string;
            baseUrl?: string;
            debug: boolean;
            model: Model;
        },
    ) {
        let baseUrl = options.baseUrl ?? BASE_URL;
        if (baseUrl.endsWith("/")) {
            baseUrl = baseUrl.slice(0, -1);
        }
        if (options.debug) {
            inner = withTrace(inner);
        }

        return async function fetchForGemini(
            path: Endpoint,
            init?: Parameters<typeof globalThis.fetch>[1] & { model?: Model },
        ) {
            init = init ?? {};

            // path to url with model
            const model: Model = init.model ?? options.model; // e.g. gemini-1.5-flash
            let url = baseUrl +
                path.replace("{model=models/*}", `models/${model}`); // e.g. *

            const u = new URL(url);
            const headers = init.headers as Record<string, string> ?? {};

            // set api key if not exists
            if (
                (typeof url === "string" && url.startsWith(baseUrl + "/")) &&
                !u.searchParams.has("key")
            ) {
                headers["Content-Type"] = "application/json";
                u.searchParams.set("key", options.apiKey);

                // stream support (SSE)
                if (path === "/v1beta/{model=models/*}:streamGenerateContent") {
                    u.searchParams.set("alt", "sse");
                }

                url = u.toString();
            }

            return await inner(url, { ...init, headers });
        };
    }
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
