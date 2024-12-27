import { HTML, tsxToJs } from "jsr:@podhmo/glue@0.1.1/mini-webapp";
import { type Context, Hono } from "jsr:@hono/hono@4.6.14";

// deno serve --port 8080 -A 00app.ts

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
    let url = `https://esm.sh${req.path}`;
    const query = req.query();
    if (Object.keys(query).length > 0) {
        url += `?${new URLSearchParams(query).toString()}`;
    }

    console.error("%cproxy request : %s", "color:gray", url);
    const startTime = performance.now();
    try {
        const res = await fetch(url, {
            method: req.method,
            headers: req.header(),
        }); // todo: cache, timeout
        console.error(
            "%cproxy response: %s, duration: %o",
            "color:gray",
            res.status,
            performance.now() - startTime,
        );
        return new Response(res.body, {
            status: res.status,
            headers: res.headers,
        });
    } catch (e) {
        console.error("proxy error: %o", e);
        return ctx.text("something wrong", { status: 500 });
    }
});

export default app; // for deno serve
