import { join as pathjoin } from "jsr:@std/path@1.0.8/join";

import { type Context, Hono } from "jsr:@hono/hono@4.6.15";
// 0.2.3だとdepsを丁寧に拾ってきてしまうので動かない
import { HTML, tsxToJs } from "jsr:@podhmo/glue@0.2.3/mini-webapp";
// import { HTML, tsxToJs } from "jsr:@podhmo/glue@0.2.2/mini-webapp";

// serve for development
// $ deno run -A jsr:@podhmo/glue@0.2.2 serve --port 8080 ./app.ts
//
// bunle to single html file
// $ deno run -A jsr:@podhmo/glue@0.2.2 bundle --output-style html --html-id app ./client.tsx > index.html

const app = new Hono();
const tsx = Deno.env.get("TSX") || "./client.tsx";
app.get("/", async (ctx: Context) => {
  const filepath = pathjoin(tsx);
  const code = await tsxToJs(filepath);
  const html = HTML({ code, id: "app", title: "Counter" });
  return ctx.html(html);
});
export default app;
