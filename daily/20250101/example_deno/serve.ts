import { debounce } from "jsr:@std/async@1.0.9/debounce";

// deno run -A serve.ts

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

// htmlファイルの一覧表示
async function handleIndex(_: Request): Promise<Response> {
  const files = Deno.readDir(".");
  const items = [];
  for await (const f of files) {
    if (f.isDirectory) {
      continue;
    }
    if (!f.name.endsWith(".html")) {
      continue;
    }

    items.push(`<li><a href="${f.name}">${f.name}</a></li>`);
  }
  const html =
    `<html><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><body class="container"><ul>${
      items.join("\n")
    }</ul></body></html>`;
  return new Response(html, {
    headers: { "Content-Type": "text/html" },
  });
}

async function handleRequest(req: Request): Promise<Response> {
  const url = new URL(req.url);

  // sse event for reload
  if (url.pathname === "/sse") {
    return handleSSE(req);
  }

  try {
    // index page
    if (url.pathname === "/") {
      return await handleIndex(req);
    }

    // favicon
    const filePath = url.pathname;
    if (filePath === "/favicon.ico") {
      return new Response("", { status: 404 });
    }

    // js
    if (filePath.endsWith(".js")) {
      const file = await Deno.readFile(`./${filePath}`);
      return new Response(file, {
        headers: { "Content-Type": "text/javascript" },
      });
    }

    // html
    let text = await Deno.readTextFile(`./${filePath}`);
    const reloadScript = `
<script type="module">
  const sse = new EventSource("/sse");

  sse.onmessage = (event) => {
      if (event.data.startsWith("reload")) {
          const parts = event.data.split(" ");
          if(parts.length < 2) {
            console.log("Reloading page...");
            location.reload();
          } else {
            console.log("Reloading file: " + parts[1]);
            location.replace(parts[1]);
          }
      }
  };
</script>`;
    text = text.replace("</body>", `${reloadScript}</body>`);
    return new Response(text, {
      headers: { "Content-Type": "text/html" },
    });
  } catch {
    return new Response("404 Not Found", { status: 404 });
  }
}

// htmlファイル変更監視と更新通知
async function watchFiles(dir: string) {
  const watcher = Deno.watchFs(dir, { recursive: true });

  const handle = debounce((ev: Deno.FsEvent) => {
    console.log(`debounced: ${ev.kind} -- ${ev.paths}`);
    for (const write of clients) {
      for (const path of ev.paths) {
        const basename = path.split("/").pop();
        if (path.endsWith(".html") && Deno.lstatSync(path).size > 0) {
          write(`data: reload ${basename}\n\n`);
        }
      }
    }
  }, 100); // 100ms

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
