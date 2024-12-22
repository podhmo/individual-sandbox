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
import { useState } from 'react';
import { jsx as _jsx } from 'react/jsx-runtime';

function Counter() {
  const [count, setCount] = useState(0);

  return _jsx('div', {
    children: [
      _jsx('p', { children: count }),
      _jsx('button', {
        onClick: () => setCount(count + 1),
        children: 'Increment'
      })
    ]
  });
}

export default function App() {
  return _jsx("main", {
    children:[
      _jsx('h1', { children: 'hello world' }),
      _jsx(Counter, {})
  ]});
}

const root = createRoot(document.getElementById('root'));
root.render(
  _jsx(StrictMode, {
    children: _jsx(App, {})
  })
);
`;

// deno serve --port 8080 -A 02app.ts
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
