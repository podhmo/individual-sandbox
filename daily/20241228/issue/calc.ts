// pick from https://github.com/denosaurs/cache/blob/83e4287694b0affe39bcad72f4cb7f99d3679057/file.ts#L125
async function hash(url: URL): Promise<string> {
    const formatted = `${url.pathname}${url.search ? "?" + url.search : ""}`;
    const encoder = new TextEncoder();
    const data = encoder.encode(formatted);
    const hash = await crypto.subtle.digest("SHA-256", data);
    return new TextDecoder().decode(hash);
}

// await Deno.lstat("hello\0world"); // same error is occured

const url = "https://esm.sh/stable/react@18";
const calculatedPath = await hash(new URL(url));
console.log("calculated path", calculatedPath);
console.log("has null byte?", calculatedPath.includes("\0")); // true
console.log(await Deno.lstat(calculatedPath)); // same error is occured

// Output:
// $ deno run -A calc.ts
// calculated path ???????????????????
// has null byte? true
// error: Uncaught (in promise) TypeError: file name contained an unexpected NUL byte: lstat '????????????????????????'
// console.log(await Deno.lstat(calculatedPath)); // same error is occured
//             ^
//     at async Object.lstat (ext:deno_fs/30_fs.js:399:15)
//     at async file:///home/po/ghq/github.com/podhmo/individual-sandbox/daily/20241228/issue/calc.ts:15:13
