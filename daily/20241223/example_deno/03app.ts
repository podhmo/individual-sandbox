import { Context, Hono } from "jsr:@hono/hono@4.6.14";

// client side code (see: 03code.tsx -> 03app.js)
const code = `
// 03code.tsx
import { StrictMode } from "https://esm.sh/react@18";
import { createRoot } from "https://esm.sh/react-dom@18/client";
import { useState } from "https://esm.sh/react@18";
import { jsx, jsxs } from "https://esm.sh/react@18/jsx-runtime";
function Counter() {
  const [count, setCount] = useState(0);
  return /* @__PURE__ */ jsxs("div", { children: [
    /* @__PURE__ */ jsx("p", { children: count }),
    /* @__PURE__ */ jsx("button", { onClick: () => setCount(count + 1), children: "Increment" })
  ] });
}
function App() {
  return /* @__PURE__ */ jsxs("main", { children: [
    /* @__PURE__ */ jsx("h1", { children: "hello world" }),
    /* @__PURE__ */ jsx(Counter, {})
  ] });
}
var root = createRoot(document.getElementById("root"));
root.render(
  /* @__PURE__ */ jsx(StrictMode, { children: /* @__PURE__ */ jsx(App, {}) })
);
export {
  App as default
};
`;

// deno serve --port 8080 -A 02app.ts
const app = new Hono();
app.get("/", (ctx: Context) => {
    const html = [
        "<!DOCTYPE html>",
        "<html>",
        "<head>",
        "<title>hello world</title>",
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
