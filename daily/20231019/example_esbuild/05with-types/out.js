import * as React from "react";
import * as Server from "react-dom/server";
export function Greet({ name }) {
  return /* @__PURE__ */ React.createElement("h1", null, "Hello, name!");
}
console.log(Server.renderToString(
  /* @__PURE__ */ React.createElement(React.StrictMode, null, /* @__PURE__ */ React.createElement(Greet, { name: "World" }))
));
