import { HTML, tsxToJs } from "jsr:@podhmo/glue@0.1.1/mini-webapp";
import { type Context, Hono } from "jsr:@hono/hono@4.6.14";

// deno serve --port 8080 -A 02app.ts
const app = new Hono();
app.get("/", async (ctx: Context) => {
    const code = await tsxToJs({ filename: "02client.tsx", debug: true });
    return ctx.html(HTML({ id: "app", code, title: "Counter example" }));
});
export default app;
