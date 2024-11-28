import { Fragment, jsx, jsxs } from "./mini-jsx/jsx-runtime";
const element = /* @__PURE__ */ jsxs("section", { className: "container", children: [
  /* @__PURE__ */ jsx("h1", { children: "Hello, World!" }),
  /* @__PURE__ */ jsx(Fragment, { children: "This is a fragment!" })
] });
console.dir(element, { depth: null });
