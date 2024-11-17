import { renderToString } from "npm:preact-render-to-string";
import { Hello } from "./dst_main5.mjs";

console.log(renderToString(Hello({ name: "World" })));