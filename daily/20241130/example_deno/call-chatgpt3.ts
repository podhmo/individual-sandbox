import { parseArgs } from "jsr:@podhmo/with-help@0.5.0"
import "jsr:@std/dotenv/load"  // .envファイルを読み込む

type ChatMessage = {
  role: "system" | "user" | "assistant";
  content: string;
};

async function fetchChatCompletion(messages: ChatMessage[]): Promise<string> {
  try {
    const response = await fetch("/v1/chat/completions", {
      method: "POST",
      body: JSON.stringify({
        model: "gpt-4o-mini", // 使用するモデルを指定 ("gpt-4", "gpt-3.5-turbo", など)
        messages: messages,
        max_tokens: 100, // 必要に応じて変更
        temperature: 0.7, // 必要に応じて変更
      }),
    });

    if (!response.ok) {
      throw new Error(`Error: ${response.status} - ${response.statusText}`);
    }

    const data = await response.json();
    const completion = data.choices[0]?.message?.content;
    if (!completion) {
      throw new Error("No completion found in the response.");
    }

    return completion;
  } catch (error) {
    console.error("Failed to fetch chat completion:", error);
    throw error;
  }
}

let DEBUG = false;

async function fetch(url: string, init?: Parameters<typeof globalThis.fetch>[1]): ReturnType<typeof globalThis.fetch> {
  init = init ?? {};

  // path to url
  if (url.startsWith("/")) {
    url = "https://api.openai.com" + url;
  }

  // set Authorization header iff url is OpenAI API
  const headers = init.headers as Record<string, string> ?? {};
  if (url.startsWith("https://api.openai.com/") && headers["Authorization"] === undefined) {
    headers["Content-Type"] = "application/json";
    headers["Authorization"] = `Bearer ${Deno.env.get("OPENAI_API_KEY")}`;
  }

  if (DEBUG) {
    console.dir({ url, method: init?.method, headers, body: init?.body }, { depth: null });
  }
  const response = await globalThis.fetch(url, { ...init, headers });
  if (DEBUG) {
    if (response.ok) {
      console.dir({ response }, { depth: null }); // 正常系のときにレスポンスを消費したくない
    } else {
      console.dir({ response, text: await response.text() }, { depth: null });
    }
  }

  return response
}


async function main() {
  const args = parseArgs(Deno.args, {
    string: ["apiKey"],
    boolean: ["debug"],
    required: ["apiKey"],
    envvar: {
      apiKey: "OPENAI_API_KEY",
      debug: "DEBUG",
    }
  });
  DEBUG = args.debug;
  Deno.env.set("OPENAI_API_KEY", args.apiKey); // 気持ち悪いけれど、fetch内でDeno.env.get("OPENAI_API_KEY")を使っているため

  const messages: ChatMessage[] = [
    { role: "system", content: "You are a helpful assistant." },
    { role: "user", content: "What is the capital of France?" },
  ];

  try {
    const completion = await fetchChatCompletion(messages);
    console.log("AI Response:", completion);
  } catch (error) {
    console.error("Error:", error);
  }
}

if (import.meta.main) {
  await main();
}
