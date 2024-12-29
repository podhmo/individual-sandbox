import { HTML, tsxToJs } from "jsr:@podhmo/glue@0.1.3/mini-webapp";
import { type Context, Hono } from "jsr:@hono/hono@4.6.15";

// $ deno run -A jsr:@podhmo/glue@0.1.3/serve --port 8080 00app.ts
const app = new Hono();
app.get("/", async (ctx: Context) => {
    const code = await tsxToJs("01client.tsx");
    return ctx.html(HTML({ code, id: "app", title: "hello" }));
});
app.get("/api/readme", async (ctx: Context) => {
    const lines = (await Deno.readTextFile("README.md")).split("\n");
    const items: string[] = [];
    for (const line of lines) {
        if (line.startsWith("## ")) {
            items.push(line.substring(3));
        }
    }
    return ctx.json({ items });
});
export default app;
