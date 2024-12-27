import { HTML, tsxToJs } from "jsr:@podhmo/glue@0.1.1/mini-webapp";
import { type Context, Hono } from "jsr:@hono/hono@4.6.14";
import * as cache from "jsr:@denosaurs/cache@0.2.15";
// import { FileWrapper } from "jsr:@denosaurs/cache@0.2.15/file";

// deno serve --port 8080 -A 02app.ts

// debug用に雑にキャッシュディレクトリを表示
console.error("cache directory: %s", cache.directory());
// cache.purge();

const app = new Hono();
app.get("/", async (ctx: Context) => {
    const code = await tsxToJs({
        filename: "./client.tsx",
        debug: true,
        baseUrl: "/stable", // 自分自身のproxy endpointにリクエストを送る
    });
    return ctx.html(HTML({ id: "app", title: "counter", code: code }));
});

// esm-shが自分自身のパスを返すのでとりあえずすべてをproxyする
//
// e.g.
// /* esm.sh - react@18.3.1 */
// export * from "/stable/react@18.3.1/es2022/react.mjs";
// export { default } from "/stable/react@18.3.1/es2022/react.mjs";

app.get("/*", async (ctx: Context): Promise<Response> => {
    const req = ctx.req;
    let url = new URL(req.path, "https://esm.sh").toString();
    const query = req.query();
    if (Object.keys(query).length > 0) {
        url += `?${new URLSearchParams(query).toString()}`; // todo: sorted query string is needed (for cache)
    }

    console.error("%cproxy request : %s", "color:gray", url);

    // confirm cache and download
    const ns = "podhmo-glue";
    const cached = await cache.cache(url, undefined, ns); // todo: passing policy
    const fileData = await Deno.readFile(cached.path);

    return new Response(fileData, {
        headers: cached.meta.headers,
        status: 200,
    });
});

export default app; // for deno serve
