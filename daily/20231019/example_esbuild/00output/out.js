import * as React from "react";
import * as Server from "react-dom/server";
let Greet = () => /* @__PURE__ */ React.createElement("h1", null, "Hello, world!");
console.log(Server.renderToString(/* @__PURE__ */ React.createElement(Greet, null)));
