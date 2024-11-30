import { parseArgs } from "jsr:@podhmo/with-help@0.5.0"
import "jsr:@std/dotenv/load"  // .envファイルを読み込む

type Fetch = typeof globalThis.fetch;

// 利用可能なエンドポイントとそのドキュメントへのリンク
const APIDoc = {
    "/v1/chat/completions": "https://beta.openai.com/docs/api-reference/completions/create",
    "/v1/embeddings": "https://beta.openai.com/docs/api-reference/embeddings/create",
} as const
type Endpoint = keyof typeof APIDoc;


/** returning fetch() funtcion for OpenAI API */
function buildFetchFunctionForOpenAIAPI(options: { apiKey: string, baseUrl?: string, debug?: boolean, fetch?: Fetch, }) {
    const inner = options.fetch ?? globalThis.fetch;
    const debug = options.debug ?? false;

    let baseUrl = options.baseUrl ?? "https://api.openai.com";
    if (baseUrl.endsWith("/")) {
        baseUrl = baseUrl.slice(0, -1);
    }

    return async function fetch(path: Endpoint, init?: Parameters<Fetch>[1]): ReturnType<Fetch> {
        init = init ?? {};

        // path to url
        const url = baseUrl + path;  // e.g. "https://api.openai.com" + url;

        // set Authorization header iff url is OpenAI API
        const headers = init.headers as Record<string, string> ?? {};
        if ((typeof url === "string" && url.startsWith(baseUrl + "/")) && headers["Authorization"] === undefined) {
            headers["Content-Type"] = "application/json";
            headers["Authorization"] = `Bearer ${options.apiKey}`;
        }

        // trace request
        if (debug) {
            console.dir({ url, method: init?.method, headers, body: init?.body }, { depth: null });
        }

        const response = await inner(url, { ...init, headers });

        // trace response
        if (response.ok) {
            if (debug) {
                console.dir({ response }, { depth: null }); // 正常系のときにレスポンスを消費したくない
            }
            return response
        } else {
            if (debug) {
                console.dir({ response, text: await response.text() }, { depth: null });
            }
            throw new Error(`Error: ${response.status} - ${response.statusText}`);
        }
    }
}


type ChatMessage = {
    role: "system" | "user" | "assistant";
    content: string;
};

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

    // ここでfetch関数を作成 (呼び間違いを避けるためにfetchとは異なる名前のほうが良いかもしれない)
    const fetch = buildFetchFunctionForOpenAIAPI({ apiKey: args.apiKey, debug: args.debug });

    const messages: ChatMessage[] = [
        { role: "system", content: "You are a helpful assistant." },
        { role: "user", content: "What is the capital of France?" },
    ];

    try {
        // see: https://beta.openai.com/docs/api-reference/completions/create
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
        const completion = data.choices[0]?.message?.content;
        if (!completion) {
            throw new Error("No completion found in the response.");
        }
        console.log("AI Response:", completion);
    } catch (error) {
        console.error("Error:", error);
    }
}

if (import.meta.main) {
    await main();
}
