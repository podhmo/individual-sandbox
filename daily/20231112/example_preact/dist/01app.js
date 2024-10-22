import { h, Fragment } from "preact";
import { signal } from "@preact/signals";
const count = signal(0);
export const App = () => {
  return /* @__PURE__ */ h(Fragment, null, /* @__PURE__ */ h("h1", null, "hello"), /* @__PURE__ */ h("button", { onClick: () => count.value++ }, "click me ", count.value));
};
