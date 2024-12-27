async function hash(url: URL): Promise<string> {
    const formatted = `${url.pathname}${url.search ? "?" + url.search : ""}`;
    const encoder = new TextEncoder();
    const data = encoder.encode(formatted);
    const hash = await crypto.subtle.digest("SHA-256", data);
    return new TextDecoder().decode(hash);
}

// await Deno.lstat("hello\0world");

const url = "https://esm.sh/stable/react@18";
const calculatedPath = await hash(new URL(url));
console.log("calculated path", calculatedPath);
console.log("has null byte?", calculatedPath.includes("\0"));
console.log(await Deno.lstat(calculatedPath));

// $ deno run -A error-example.ts
// calculated path /home/po/.cache/deno/podhmo-glue/https/esm.sh/�g��E=m�Y:M���t ���-�Ƀ�c���j
// error: Uncaught (in promise) TypeError: file name contained an unexpected NUL byte: lstat '/home/po/.cache/deno/podhmo-glue/https/esm.sh/�g��E=m�Y:M���t ���-�Ƀ�c���j'
// console.log(await Deno.lstat(calculatedPath));
//             ^
//     at async Object.lstat (ext:deno_fs/30_fs.js:399:15)
//     at async file:///home/po/ghq/github.com/podhmo/individual-sandbox/daily/20241228/error-example.ts:28:13
