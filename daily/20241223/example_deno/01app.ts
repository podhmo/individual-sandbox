import { Context, Hono } from "jsr:@hono/hono@4.6.14";

// import map for client side code
const importMap = {
    imports: {
        react: "https://esm.sh/react@18",
        "react/": "https://esm.sh/react@18/",
        "react-dom/client": "https://esm.sh/react-dom@18/client",
    },
};

// client side code
const code = `
import { StrictMode } from 'react';
import { createRoot } from 'react-dom/client';
import { jsx as _jsx } from 'react/jsx-runtime';

const root = createRoot(document.getElementById('root'));
root.render(
  _jsx(StrictMode, {
    children: _jsx('h1', { children: 'hello world' })
  })
);
`;

// deno serve --port 8080 -A 01app.ts
const app = new Hono();
app.get("/", (ctx: Context) => {
    const html = [
        "<!DOCTYPE html>",
        "<html>",
        "<head>",
        "<title>hello world</title>",
        `<script type="importmap">`,
        JSON.stringify(importMap, null, 2),
        `</script>`,
        "</head>",
        "<body>",
        "<div id='root'></div>",
        "<script type='module'>",
        code,
        "</script>",
        "</body>",
        "</html>",
    ].join("\n");
    return ctx.html(html);
});
export default app; // for deno serve
