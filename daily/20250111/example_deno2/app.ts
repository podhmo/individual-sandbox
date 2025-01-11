import { join as pathjoin } from "jsr:@std/path@1.0.8/join";

import { type Context, Hono } from "jsr:@hono/hono@4.6.15";
import { HTML, tsxToJs } from "jsr:@podhmo/glue@0.2.3/mini-webapp";

// serve for development
// $ deno run -A jsr:@podhmo/glue@0.2.3 serve --port 8080 ./app.ts
//
// bunle to single html file
// $ deno run -A jsr:@podhmo/glue@0.2.3 bundle --output-style html --html-id root ./client.tsx > index.html
//

const app = new Hono();
app.get("/", async (ctx: Context) => {
  const filename = Deno.env.get("TSX") || "client.tsx";
  const filepath = pathjoin(import.meta.dirname ?? "", filename);
  const title = Deno.env.get("TITLE") || "Counter";

  const code = await tsxToJs(filepath);
  const html = HTML({ code, id: "root", title });
  return ctx.html(html);
});
export default app;
