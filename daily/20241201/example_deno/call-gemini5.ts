import { moreStrict, parseArgs } from "jsr:@podhmo/with-help@0.5.1";
import "jsr:@std/dotenv/load";
import { events } from "jsr:@lukeed/fetch-event-stream@0.1.5";

export const BASE_URL = "https://generativelanguage.googleapis.com";

interface DefaultOptions {
    model: Model;
}

const defaultOptions: DefaultOptions = {
    model: "gemini-1.5-flash",
};

interface BaseOptions {
    apiKey: string;
    baseUrl: string;
    debug: boolean;
}

async function main() {
    const helpText = `Usage: gemini-client [options] <subcommand> [args]
Options:
  --apiKey     <string> (required) (env: GEMINI_API_KEY)
  --baseUrl    <string> (required) (default: baseUrl=${BASE_URL})
  --debug      (default: debug=false)    (env: DEBUG)
  --help       show help

Available subcommands:
  list-model -- listing models
  chat       -- chat with model`;

    const baseOptions = parseArgs(Deno.args, {
        name: "gemini-client",
        string: ["apiKey", "baseUrl"],
        boolean: ["debug"],
        required: ["apiKey", "baseUrl"],
        default: {
            baseUrl: BASE_URL,
        },
        envvar: {
            apiKey: "GEMINI_API_KEY",
            debug: "DEBUG",
        },
        stopEarly: true,
        helpText: helpText,
    });

    const rest = baseOptions._;
    switch (rest[0]) {
        case "list-model":
            return await listModelCommand(rest.slice(1), baseOptions);
        case "chat":
            return await chatCommand(rest.slice(1), baseOptions);
        default: {
            // FIXME: helpTextをここで表示するためにはhelpTextを変数に持つ必要がある
            console.error(helpText);
            console.error("");
            console.error(`Error: unknown subcommand ${rest[0] ?? ""}`);
            Deno.exit(1);
        }
    }
}

async function listModelCommand(cliArgs: string[], baseOptions: BaseOptions) {
    const formats = ["text", "json"] as const;
    const defaultFormat = "text" as const;

    const options0 = parseArgs(cliArgs, {
        name: "list-model",
        string: ["format"],
        boolean: ["debug"],
        required: ["format"],
        default: {
            format: defaultFormat,
        },
        flagDescription: {
            "format": `one of ${
                JSON.stringify(formats)
            }, (default: format=${defaultFormat})`,
        },
    });

    const choices = moreStrict(options0).choices;
    const options = { ...options0, format: choices(options0.format, formats) };

    const fetch = buildFetch(globalThis.fetch, {
        ...defaultOptions,
        ...baseOptions,
        ...options,
    });

    try {
        const response = await fetch("/v1beta/models");
        const data = await response.json(); // todo: typing

        switch (options.format) {
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
                const _: never = options.format;
                throw new Error("unexpected format");
            }
        }
    } catch (error) {
        console.error("Error:", error);
    }
}

async function chatCommand(cliArgs: string[], baseOptions: BaseOptions) {
    const defaultModel = defaultOptions.model;
    const options0 = parseArgs(cliArgs, {
        name: "chat",
        string: ["model"],
        required: ["model"],
        boolean: ["debug"],
        default: {
            model: defaultModel,
        },
        flagDescription: {
            "model": `one of ${
                JSON.stringify(models)
            }, (default: model=${defaultModel})`,
        },
    });

    const choices = moreStrict(options0).choices;
    const options = { ...options0, model: choices(options0.model, models) };

    const fetch = buildFetch(globalThis.fetch, {
        ...defaultOptions,
        ...baseOptions,
        ...options,
    });

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
                model: options.model,
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
