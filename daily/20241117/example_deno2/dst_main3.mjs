// hello.ts
import { chunk } from "https://esm.sh/jsr/@std/collections@1.0.9/chunk";
function hello(name) {
  return `Hello, ${name}!`;
}

// src_main3.ts
console.log(hello("world"));
console.log(chunk([1, 2, 3, 4, 5], 2));
