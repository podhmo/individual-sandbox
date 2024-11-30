import { parseArgs } from "jsr:@podhmo/with-help@0.5.0"
import "jsr:@std/dotenv/load"

async function main() {
    const args = parseArgs(Deno.args, {
        string: ["apiKey", "baseUrl"],
        boolean: ["debug"],
        required: ["apiKey"],
        default: {
            baseUrl: BASE_URL,
        },
        envvar: {
            apiKey: "GEMINI_API_KEY",
            debug: "DEBUG",
        }
    });

    const fetch = buildFetch(globalThis.fetch, args);

    try {
        // see: https://ai.google.dev/gemini-api/docs/api-key?hl=ja
        const payload = { "contents": [{ "parts": { "text": "Write a story about a magic backpack." } }] }
        const response = await fetch("/v1beta/models/${model}:generateContent", {
            method: "POST",
            body: JSON.stringify(payload),
            model: "gemini-1.5-flash-8b",
        });

        // 本来はいい感じに取り出す
        const data = await response.json();
        const text = data["candidates"][0]["content"]["parts"][0]["text"];

        console.log("AI Response:", text);
    } catch (error) {
        console.error("Error:", error);
    }
}

////////////////////////////////////////
// internal library
////////////////////////////////////////

// path -> document url
interface APIDoc {
    "/v1beta/models/${model}:generateContent": "https://ai.google.dev/gemini-api/docs/text-generation"
}

// model https://ai.google.dev/gemini-api/docs/models/gemini?hl=ja
const models = ["gemini-1.5-flash", "gemini-1.5-flash-8b", "gemini-1.5-pro"] as const;
type Model = typeof models[number];

export type Endpoint = keyof APIDoc;
export const BASE_URL = "https://generativelanguage.googleapis.com";

/** the specialized version of fetch() function for Gemini */
export function buildFetch(inner: typeof globalThis.fetch, options: { apiKey: string, baseUrl?: string, debug: boolean }) {
    let baseUrl = options.baseUrl ?? BASE_URL;
    if (baseUrl.endsWith("/")) {
        baseUrl = baseUrl.slice(0, -1);
    }
    if (options.debug) {
        inner = withTrace(inner);
    }

    return async function fetchForGemini(path: Endpoint, init?: Parameters<typeof globalThis.fetch>[1] & {model?: Model}) {
        init = init ?? {};
    
        // path to url with model
        const model: Model = init.model ?? "gemini-1.5-flash"; // default model
        let url = baseUrl + path.replace("${model}", model);; // e.g. *

        const u = new URL(url);
        const headers = init.headers as Record<string, string> ?? {};

        // set api key if not exists
        if ((typeof url === "string" && url.startsWith(baseUrl + "/")) && !u.searchParams.has("key")) {
            headers["Content-Type"] = "application/json";
            u.searchParams.set("key", options.apiKey);
            url = u.toString();
        }

        return await inner(url, { ...init, headers });
    }
}

export function withTrace(inner: typeof globalThis.fetch) {
    return async function fetchWithTrace(url: Parameters<typeof globalThis.fetch>[0], init?: Parameters<typeof globalThis.fetch>[1]) {
        const headers = init?.headers as Record<string, string> ?? {};

        // trace request
        console.error({ url, method: init?.method, headers, body: init?.body });

        const response = await inner(url, { ...init, headers });

        // trace response
        if (!response.ok) {
            console.error({ response, text: await response.text() });
            throw new Error(`Error: ${response.status} - ${response.statusText}`); // ここで例外を投げないとresponseを後続で消費されてしまう
        }
        console.error({ response }); // 正常系のときにレスポンスを消費したくない
        return response
    }
}

if (import.meta.main) {
    await main();
}
