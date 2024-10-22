import * as React from "react";
import * as Server from "react-dom/server";
let Greet = () => /* @__PURE__ */ h("h1", null, "Hello, world!");
console.log(Server.renderToString(/* @__PURE__ */ h(Greet, null)));
