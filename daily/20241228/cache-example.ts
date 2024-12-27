import * as cache from "jsr:@denosaurs/cache@0.2.15";
import { FileWrapper } from "jsr:@denosaurs/cache@0.2.15/file";

// $ deno run -A cache-example.ts

const ns = "podhmo-glue";
console.log("cache directory", cache.directory());

// OK:  const url = "https://esm.sh/react@18.3.1/es2022/react.mjs";
// OK: const url = "https://esm.sh/stable/react@18";

// 謎。このURLのときだけNULLバイトが含まれていると言われる
// // filepath:  /home/po/.cache/deno/podhmo-glue/https/esm.sh/�g��E=m�Y:M���t ���-�Ƀ�c���j
// error: Uncaught (in promise) TypeError: file name contained an unexpected NUL byte: lstat '/home/po/.cache/deno/podhmo-glue/https/esm.sh/�g��E=m�Y:M���t ���-�Ƀ�c���j'
// await Deno.lstat(filePath);
const url = "https://esm.sh/stable/react@18";

// debug
const wrapper = await FileWrapper.create(new URL(url), undefined, ns);
console.log("filepath: ", wrapper.path);
console.log("exists: ", await wrapper.exists());

// use cache
const fs = await cache.cache(url, undefined, ns);
console.log("cached: %o %o", fs.path, fs.meta);
