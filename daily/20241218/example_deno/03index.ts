import { type Context, Hono } from "jsr:@hono/hono@4.6.14";

import { serveStatic } from "jsr:@hono/hono@4.6.14/deno";

const page = `<!DOCTYPE html>
<html>

<head>
    <title>Home</title>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
</head>

<body>
    <main id="app">
    </main>
    <script type="module">
        import { render } from "https://esm.sh/jsr/@hono/hono@4.6.14/jsx/dom/";
        import { Top } from "/js/app.js";

        const root = document.getElementById("app");
        render(Top({ name: "deno" }), root);
    </script>
</body>
</html>`;

const app = new Hono();
app.get("/", (c: Context) => {
    return c.html(page);
});
app.use("/js/app.js", serveStatic({ path: "./03components.js" }));
export default app;
