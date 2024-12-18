import { type Context, Hono } from "jsr:@hono/hono@4.6.14";

// deno serve --allow-net --port 8080 00hello.ts
const app = new Hono();
app.get("/hello/:name", (c: Context): Response => {
    const name = c.req.param("name");
    return c.json({ message: `Hello, ${name}!` });
});
export default app;
