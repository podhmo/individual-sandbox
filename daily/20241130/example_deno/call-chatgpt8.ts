import { parseArgs } from "jsr:@podhmo/with-help@0.5.0"
import "jsr:@std/dotenv/load"  // .envファイルを読み込む

type _Fetch = typeof globalThis.fetch;

export function withTrace<T extends _Fetch>(inner: T): T & { "trace": true } {
    const outer = async function traceFetch(url: Parameters<_Fetch>[0], init?: Parameters<_Fetch>[1]) {
        const headers = init?.headers as Record<string, string> ?? {};
        // trace request
        console.error({ url, method: init?.method, headers, body: init?.body });

        const response = await inner(url, { ...init, headers });

        // trace response
        if (response.ok) {
            console.error({ response }); // 正常系のときにレスポンスを消費したくない
            return response
        } else {
            console.error({ response, text: await response.text() });
            throw new Error(`Error: ${response.status} - ${response.statusText}`);
        }
    }
    return outer as T & { "trace": true };
}


export function withOpenAI<T extends _Fetch>(inner: T, options: { apiKey: string, baseUrl?: string }): T & { "openai": true } {
    let baseUrl = options.baseUrl ?? "https://api.openai.com";
    if (baseUrl.endsWith("/")) {
        baseUrl = baseUrl.slice(0, -1);
    }

    const outer = async function fetch(url: Parameters<_Fetch>[0], init?: Parameters<_Fetch>[1]) {
        init = init ?? {};

        // path to url
        if (typeof url === "string" && url.startsWith("/")) {
            url = baseUrl + url; // e.g. "https://api.openai.com" + url;
        }

        // set Authorization header iff url is OpenAI API
        const headers = init.headers as Record<string, string> ?? {};
        if ((typeof url === "string" && url.startsWith(baseUrl + "/")) && headers["Authorization"] === undefined) {
            headers["Content-Type"] = "application/json";
            headers["Authorization"] = `Bearer ${options.apiKey}`;
        }

        const response = await inner(url, { ...init, headers });

        if (!response.ok) {
            throw new Error(`Error: ${response.status} - ${response.statusText}`);

        }
        return response
    }
    return outer as T & { "openai": true };
}


// 利用可能なエンドポイントとそのドキュメントへのリンク
const APIDoc = {
    "/v1/chat/completions": "https://beta.openai.com/docs/api-reference/completions/create",
    "/v1/embeddings": "https://beta.openai.com/docs/api-reference/embeddings/create",
} as const
type Endpoint = keyof typeof APIDoc;

type NarrowFirstArgument<
    // deno-lint-ignore no-explicit-any
    F extends (arg: string, ...args: any[]) => unknown, // 引数が文字列を受け取る任意の関数型
    T extends string // 絞り込みに使用するリテラル型
> = (arg: T, ...args: Parameters<F> extends [unknown, ...infer Rest] ? Rest : never) => ReturnType<F>;


export type Fetch = NarrowFirstArgument<_Fetch, Endpoint> & { "openai": true };

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

    // ここの定義を手続き的に書くと型の推論がうまくいかない
    const fetch = withOpenAI((args.debug ? withTrace(globalThis.fetch) : globalThis.fetch), args);

    const messages: ChatMessage[] = [
        { role: "system", content: "You are a helpful assistant." },
        { role: "user", content: "What is the capital of France?" },
    ];

    try {
        const completion = await chat(fetch, messages);
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
