import { cache } from "jsr:@denosaurs/cache@0.2.15";

// deno run -A error.ts

const ns = "";
const url = "https://esm.sh/stable/react@18";
const fs = await cache(url, undefined, ns);
console.log("cached: %o %o", fs.path, fs.meta);
