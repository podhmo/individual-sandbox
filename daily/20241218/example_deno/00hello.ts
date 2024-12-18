import { type Context, Hono } from "jsr:@hono/hono@4.6.14";

// deno serve --allow-net --port 8080 00hello.ts
const app = new Hono();
app.get("/", (c: Context): Response => {
    return c.json({ message: "Hello, World!" });
});
export default app;
