import { h, Fragment } from "preact";
import { useState } from "preact/hooks";
export const App = () => {
  const [count, setCount] = useState(0);
  return /* @__PURE__ */ h(Fragment, null, /* @__PURE__ */ h("h1", null, "hello"), /* @__PURE__ */ h("button", { onClick: () => setCount(() => count + 1) }, "click me ", count));
};
