/** これが個人的な最終稿 
 * 
 * https://gist.github.com/podhmo/33c1452b24105a4b2123ba068d082308 で色々考えたけれど、これくらいで十分な気がする。
 * 組み込みの関数と名前が等しいfetch()をそのまま使い続けるかは議論の余地がある。
*/

import { parseArgs } from "jsr:@podhmo/with-help@0.5.0" // いい感じにhelp messageを生成したりしてくれる
import "jsr:@std/dotenv/load"  // .envファイルを読み込む

// deno-lint-ignore no-empty-interface
interface APIDoc {
    // 利用可能なエンドポイントとそのドキュメントへのリンク (必要になったら追加していく)
    // typescriptのinterfaceはopenなので、後から追加できる
}

async function main() {
    const args = parseArgs(Deno.args, {
        string: ["apiKey", "baseUrl"],
        boolean: ["debug"],
        required: ["apiKey"],
        default: {
            baseUrl: "https://api.openai.com",
        },
        envvar: {
            apiKey: "OPENAI_API_KEY",
            debug: "DEBUG",
        }
    });

    const fetch = buildFetch(globalThis.fetch, args); // import { buildFetch } from "./core.ts";
    const messages: ChatMessage[] = [
        { role: "system", content: "You are a helpful assistant." },
        { role: "user", content: "What is the capital of France?" },
    ];

    try {
        const completion = await chat(fetch, messages); // import { chat } from "./chat.ts";
        if (!completion) {
            throw new Error("No completion found in the response.");
        }
        console.log("AI Response:", completion);
    } catch (error) {
        console.error("Error:", error);
    }
}

////////////////////////////////////////
// api client
////////////////////////////////////////

// chat.ts などに分ける

interface APIDoc {
    "/v1/chat/completions": "https://beta.openai.com/docs/api-reference/completions/create",
}

export type Fetch = ReturnType<typeof buildFetch>

type ChatMessage = {
    role: "system" | "user" | "assistant";
    content: string;
};

async function chat(fetch: Fetch, messages: ChatMessage[]): Promise<string> {
    const response = await fetch("/v1/chat/completions", {
        method: "POST",
        body: JSON.stringify({
            model: "gpt-4o-mini", // 使用するモデルを指定 ("gpt-4", "gpt-3.5-turbo", など)
            messages: messages,
            max_tokens: 100, // 必要に応じて変更
            temperature: 0.7, // 必要に応じて変更
        }),
    });

    const data = await response.json();
    return data.choices[0]?.message?.content;
}

// -- embedings 用の関数が欲しくなったら追加する
//
// embedings.ts などに分ける
// interface APIDoc {
//     "/v1/embeddings": "https://beta.openai.com/docs/api-reference/embeddings/create",
// }


////////////////////////////////////////
// internal library
////////////////////////////////////////

// core.ts とか fetch.ts などに分けるかもしれない
export type Endpoint = keyof APIDoc;

/** the specialized version of fetch() function for ChatGPT */
export function buildFetch(inner: typeof globalThis.fetch, options: { apiKey: string, baseUrl?: string, debug: boolean }) {
    let baseUrl = options.baseUrl ?? "https://api.openai.com";
    if (baseUrl.endsWith("/")) {
        baseUrl = baseUrl.slice(0, -1);
    }

    return async function fetchForChatGPT(path: Endpoint, init?: Parameters<typeof globalThis.fetch>[1]) {
        init = init ?? {};

        // path to url
        const url = baseUrl + path; // e.g. "https://api.openai.com" + url;

        // set Authorization header iff url is OpenAI API
        const headers = init.headers as Record<string, string> ?? {};
        if ((typeof url === "string" && url.startsWith(baseUrl + "/")) && headers["Authorization"] === undefined) {
            headers["Content-Type"] = "application/json";
            headers["Authorization"] = `Bearer ${options.apiKey}`;
        }

        // trace request
        if (options.debug) {
            console.error({ url, method: init?.method, headers, body: init?.body });
        }

        const response = await inner(url, { ...init, headers });

        // trace response
        if (!response.ok) {
            if (options.debug) {
                console.error({ response, text: await response.text() });
            }
            throw new Error(`Error: ${response.status} - ${response.statusText}`);
        }
        if (options.debug) {
            console.error({ response }); // 正常系のときにレスポンスを消費したくない
        }

        return response
    }
}

if (import.meta.main) {
    await main();
}
