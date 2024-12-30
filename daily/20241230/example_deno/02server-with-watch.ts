import { debounce } from "jsr:@std/async@1.0.9/debounce";

// deno run -A 02server-with-watch.ts

const clients = new Set<(_: string) => void>();

function handleSSE(req: Request): Response {
    const stream = new ReadableStream<Uint8Array>({
        start(controller) {
            const writeFunc = (data: string) => {
                controller.enqueue(new TextEncoder().encode(data));
            };
            clients.add(writeFunc);
            req.signal.addEventListener("abort", () => {
                clients.delete(writeFunc);
                controller.close();
            });
        },
    });

    return new Response(stream, {
        headers: {
            "Content-Type": "text/event-stream; charset=utf-8",
            "Cache-Control": "no-cache",
            "Connection": "keep-alive",
        },
    });
}

// リクエスト処理
async function handleRequest(req: Request): Promise<Response> {
    const url = new URL(req.url);

    if (url.pathname === "/sse") {
        return handleSSE(req);
    }

    try {
        const filePath = url.pathname === "/" ? "/02.html" : url.pathname;
        const file = await Deno.readFile(`./${filePath}`);
        const contentType = filePath.endsWith(".js")
            ? "text/javascript"
            : "text/html";

        return new Response(file, {
            headers: { "Content-Type": contentType },
        });
    } catch {
        return new Response("404 Not Found", { status: 404 });
    }
}

// ファイル変更監視と更新通知
async function watchFiles(dir: string) {
    const watcher = Deno.watchFs(dir, { recursive: true });

    const handle = debounce((ev: Deno.FsEvent) => {
        console.log(`debounced: %o`, ev);
        for (const write of clients) {
            write(`data: reload\n\n`);
        }
    }, 200); // 200ms

    for await (const event of watcher) {
        handle(event);
    }
}

// サーバー起動
const ac = new AbortController();
const server = Deno.serve({
    port: 8080,
    hostname: "localhost",
    handler: handleRequest,
    signal: ac.signal,
    onListen: (addr) => {
        console.log(`listening on http://${addr.hostname}:${addr.port}`);
    },
});
server.finished.then(() => console.log("server closed"));

console.log("watching...");
// ファイル監視開始
watchFiles(".");
