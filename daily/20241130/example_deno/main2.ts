import { parseArgs } from "jsr:@podhmo/with-help@0.5.0"
import "jsr:@std/dotenv/load"

// 初手はこんな感じそう

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

    const fetch = buildFetch(globalThis.fetch, args);

    try {
        const model: Model = "gpt-4o-mini";

        // TODO: fileから読み込む (jsonschemaとかつけるかも？)
        const messages: ChatMessage[] = [
            { role: "system", content: "You are a helpful assistant." },
            { role: "user", content: "What is the capital of France?" },
        ];

        const response = await fetch("/v1/chat/completions", {
            method: "POST",
            body: JSON.stringify({
                model: model,
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

////////////////////////////////////////
// internal library
////////////////////////////////////////

// path -> document url
interface APIDoc {
    "/v1/chat/completions": "https://beta.openai.com/docs/api-reference/completions/create",
}

// model: https://platform.openai.com/docs/models#model-endpoint-compatibility
const models = ["chatgpt-4o-latest", "gpt-4o", "gpt-4o-mini", "gpt-4", "gpt-3.5-turbo"] as const;
export type Model = typeof models[number];

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
