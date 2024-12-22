import { Context, Hono } from "jsr:@hono/hono@4.6.14";

// deno serve --port 8080 -A 00app.ts
const app = new Hono();

app.get("/", (ctx: Context) => {
    return ctx.html(`<h1>hello world</h1>`);
});

export default app; // for deno serve
