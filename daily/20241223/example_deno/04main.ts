import { transform } from "./transform.ts";

const code = await transform({
  debug: true,
  filename: "03code.tsx",
});
console.log(code);
