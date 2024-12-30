// $ deno run -A --watch-hmr 00watch.ts
let i = 10;
while (true) {
    console.log("Hello World", i);
    i++;
    await new Promise((resolve) => setTimeout(() => resolve(true), 1000));
}
