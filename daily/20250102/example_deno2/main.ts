import { type Context, Hono } from "jsr:@hono/hono@4.6.15";
import { HTML, tsxToJs } from "jsr:@podhmo/glue@0.2.1/mini-webapp";

// serve for development
// $ TSX=./client.tsx deno run -A jsr:@podhmo/glue@0.2.1 serve --port 8080 ./main.ts

const app = new Hono();
app.get("/", async (ctx: Context) => {
  const code = await tsxToJs(Deno.env.get("TSX") || "./00client.tsx");
  const html = HTML({ code, id: "app", title: "-" });
  return ctx.html(html);
});
export default app;
