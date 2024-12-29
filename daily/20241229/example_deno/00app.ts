import { HTML, tsxToJs } from "jsr:@podhmo/glue@0.1.3/mini-webapp";
import { type Context, Hono } from "jsr:@hono/hono@4.6.15";

// $ deno run -A jsr:@podhmo/glue@0.1.3/serve --port 8080 00app.ts
const app = new Hono();
app.get("/", async (ctx: Context) => {
    const code = await tsxToJs("00client.tsx");
    return ctx.html(HTML({ code, id: "app", title: "hello" }));
});
export default app;
