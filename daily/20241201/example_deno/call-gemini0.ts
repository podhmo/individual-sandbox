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
        const response = await fetch("/v1beta/models/gemini-1.5-flash:generateContent", {
            method: "POST",
            body: JSON.stringify(payload),
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
    // TODO: gemini-1.5-flash 以外のモデルも対応する
    "/v1beta/models/gemini-1.5-flash:generateContent": "https://ai.google.dev/gemini-api/docs/text-generation"
}


export type Endpoint = keyof APIDoc;
export const BASE_URL = "https://generativelanguage.googleapis.com";

/** the specialized version of fetch() function for Gemini */
export function buildFetch(inner: typeof globalThis.fetch, options: { apiKey: string, baseUrl?: string, debug: boolean }) {
    let baseUrl = options.baseUrl ?? BASE_URL;
    if (baseUrl.endsWith("/")) {
        baseUrl = baseUrl.slice(0, -1);
    }

    return async function fetchForGemini(path: Endpoint, init?: Parameters<typeof globalThis.fetch>[1]) {
        init = init ?? {};

        // path to url
        let url = baseUrl + path; // e.g. *

        const u = new URL(url);
        const headers = init.headers as Record<string, string> ?? {};

        // set api key if not exists
        if ((typeof url === "string" && url.startsWith(baseUrl + "/")) && !u.searchParams.has("key")) {
            headers["Content-Type"] = "application/json";
            const u = new URL(url);
            u.searchParams.set("key", options.apiKey);
            url = u.toString();
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
