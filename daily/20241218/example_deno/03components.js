// 03components.tsx
import { useState } from "https://esm.sh/jsr/@hono/hono@4.6.14/jsx";
import { Fragment, jsx, jsxs } from "https://esm.sh/jsr/@hono/hono@4.6.14/jsx/dom/jsx-runtime";
var Top = (props) => {
  const [name, setName] = useState(props.name || "world");
  console.log(name);
  return /* @__PURE__ */ jsxs(Fragment, { children: [
    /* @__PURE__ */ jsxs("h1", { children: [
      "hello ",
      name
    ] }),
    /* @__PURE__ */ jsx("input", { value: name, onChange: (e) => setName(e.target.value) }),
    /* @__PURE__ */ jsx("h2", { children: "state" }),
    /* @__PURE__ */ jsxs("pre", { children: [
      "name: ",
      name
    ] })
  ] });
};
export {
  Top
};
