import { moreStrict, parseArgs } from "jsr:@podhmo/with-help@0.5.1";
import "jsr:@std/dotenv/load";
import { events } from "jsr:@lukeed/fetch-event-stream@0.1.5";

async function main() {
    const showHelp = () => {
        console.error("Usage: gemini-client");
        console.log("")
        console.error("Available subcommands:");
        console.error("  list-model -- listing models");
        console.error("  chat       -- chat with model");
        Deno.exit(1);
    };

    if (Deno.args.length === 0) {
        showHelp();
    }

    switch (Deno.args[0]) {
        case "list-model":
            return await listModelCommand(Deno.args.slice(1));
        case "chat":
            return await chatCommand(Deno.args.slice(1));
        default: {
            showHelp();
        }
    }
}

async function listModelCommand(cliArgs: string[]) {
    const formats = ["text", "json"] as const;
    type Format = typeof formats[number];
    const defaultFormat = "text" as const;

    const args0 = parseArgs(cliArgs, { // ほとんどの引数は共通なので毎回書くのは面倒かもしれない
        name: "list-model",
        string: ["apiKey", "baseUrl", "format"],
        boolean: ["debug"],
        required: ["apiKey", "format"],
        default: {
            baseUrl: BASE_URL,
            format: defaultFormat,
        },
        envvar: {
            apiKey: "GEMINI_API_KEY",
            debug: "DEBUG",
        },
        flagDescription: {
            "format": `one of ${
                JSON.stringify(formats)
            }, (default: format=${defaultFormat})`,
        },
    });

    const choices = moreStrict(args0).choices;
    const args = { ...args0, format: choices(args0.format, formats) };

    const fetch = buildFetch(globalThis.fetch, {
        ...args,
        model: "gemini-1.5-flash-8b", // modelが必要になるのは微妙かもしれない
    });

    try {
        const response = await fetch("/v1beta/models");
        const data = await response.json(); // todo: typing

        switch (args.format) {
            case "json":
                console.log(JSON.stringify(data, null, 2));
                break;
            case "text":
                for (const m of data.models) {
                    console.log(
                        `${m.name}: ${m.displayName}\n\t${m.description}`,
                    );
                }
                break;
            default: {
                const _: never = args.format;
                throw new Error("unexpected format");
            }
        }
    } catch (error) {
        console.error("Error:", error);
    }
}

async function chatCommand(cliArgs: string[]) {
    const defaultModel = "gemini-1.5-flash" as const;
    const args0 = parseArgs(cliArgs, {
        name: "chat",
        string: ["apiKey", "baseUrl", "model"],
        boolean: ["debug"],
        required: ["apiKey", "model"],
        default: {
            baseUrl: BASE_URL,
            model: defaultModel,
        },
        envvar: {
            apiKey: "GEMINI_API_KEY",
            debug: "DEBUG",
        },
        flagDescription: {
            "model": `one of ${
                JSON.stringify(models)
            }, (default: model=${defaultModel})`,
        },
    });

    const choices = moreStrict(args0).choices;
    const args = { ...args0, model: choices(args0.model, models) };

    const fetch = buildFetch(globalThis.fetch, args);

    try {
        // see: https://ai.google.dev/gemini-api/docs/api-key?hl=ja
        // https://github.com/lukeed/fetch-event-stream
        const abort = new AbortController();

        const payload = {
            "contents": [{
                "parts": { "text": "Write a story about a magic backpack." },
            }],
        };
        const response = await fetch(
            "/v1beta/{model=models/*}:streamGenerateContent",
            {
                method: "POST",
                signal: abort.signal,
                body: JSON.stringify(payload),
                model: args.model,
            },
        );

        const stream = events(response, abort.signal);
        for await (const event of stream) {
            const data = JSON.parse(event.data ?? "null");
            console.log(JSON.stringify(data));
        }
    } catch (error) {
        console.error("Error:", error);
    }
}

////////////////////////////////////////
// internal library
////////////////////////////////////////

// path -> document url
interface APIDoc {
    "/v1beta/{model=models/*}:generateContent":
        "https://ai.google.dev/gemini-api/docs/text-generation";
    "/v1beta/{model=models/*}:streamGenerateContent":
        "https://ai.google.dev/api/generate-content?hl=ja#v1beta.models.streamGenerateContent";
    "/v1beta/models":
        "https://ai.google.dev/api/models?hl=ja#method:-models.list";
}

// model https://ai.google.dev/gemini-api/docs/models/gemini?hl=ja
export const models = [
    "gemini-1.5-flash",
    "gemini-1.5-flash-8b",
    "gemini-1.5-pro",
] as const;
export type Model = typeof models[number];

export type Endpoint = keyof APIDoc;
export const BASE_URL = "https://generativelanguage.googleapis.com";

/** the specialized version of fetch() function for Gemini */
export function buildFetch(
    inner: typeof globalThis.fetch,
    options: { apiKey: string; baseUrl?: string; debug: boolean; model: Model },
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
        let url = baseUrl + path.replace("{model=models/*}", `models/${model}`); // e.g. *

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
