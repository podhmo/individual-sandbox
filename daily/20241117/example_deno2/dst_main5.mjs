// src_main5.tsx
import { h } from "https://esm.sh/preact@10.24.3";
function Code({ children, language }) {
  return /* @__PURE__ */ h("pre", { ...language ? { language } : {} }, children);
}
var Hello = ({ name = "<Unknown>" }) => {
  return /* @__PURE__ */ h("h1", null, "Hello ", name, "!!");
};
export {
  Code,
  Hello
};
