// component.tsx
import { render, h, Fragment } from "https://esm.sh/preact@10.24.3";
function Code({ children, language }) {
  return /* @__PURE__ */ h("pre", { ...language ? { language } : {} }, children);
}
var Hello = ({ name }) => {
  return /* @__PURE__ */ h("h1", null, "Hello ", name, "!!");
};
export {
  Code,
  Fragment,
  Hello,
  h,
  render
};
