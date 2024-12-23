/** @jsxImportSource jsr:@hono/hono@4.6.14/jsx */
import { Context, Hono } from "jsr:@hono/hono@4.6.14";
import { transform } from "./transform.ts";

// 本当は以下の様なことがしたかった。deno serveとdeno runで分岐ができない。
// deno run -A 05app.tsx                # debug print clientSideCode
// deno serve --port 8080 -A 05app.tsx  # serve http://localhost:8080

const clientSideCode = await transform({
  debug: true,
  denoConfigPath: "deno.json",
  filename: "03code.tsx",
});

const app = new Hono();
app.get("/", (ctx: Context) => {
  // https://hono.dev/docs/guides/jsx#dangerouslysetinnerhtml

  const html = (
    <html>
      <head>
        <meta charset="utf-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <title>hello world</title>
      </head>

      <body>
        <main id="root">
          <h1>...</h1>
        </main>
        <script
          type="module"
          dangerouslySetInnerHTML={{ __html: clientSideCode }}
        >
        </script>
      </body>
    </html>
  );
  return ctx.html(html);
});
export default app; // for deno serve

// only for deno run (not for deno serve)
if (import.meta.main && Deno.env.has("DEBUG")) {
    console.log(clientSideCode); // debug print
}
