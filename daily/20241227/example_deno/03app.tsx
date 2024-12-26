/** @jsxRuntime automatic */
/** @jsxImportSource jsr:@hono/hono@4.6.14/jsx */
import { Hono } from "jsr:@hono/hono@4.6.14";
import { tsxToJs } from "jsr:@podhmo/glue/mini-webapp";

type Env = {
    Bindings: {
        MY_VAR: string;
    };
};

const app = new Hono<Env>();

app.get("/api/clock", (c) => {
    return c.json({
        var: c.env.MY_VAR, // Cloudflare Bindings
        time: new Date().toLocaleTimeString(),
    });
});

app.get("*", async (c) => {
    const code = await tsxToJs({ filename: "03client.tsx", debug: true });
    return c.html(
        <html>
            <head>
                <meta charSet="utf-8" />
                <meta
                    content="width=device-width, initial-scale=1"
                    name="viewport"
                />
                <link
                    rel="stylesheet"
                    href="https://cdn.simplecss.org/simple.min.css"
                />
                <script type="module" dangerouslySetInnerHTML={{__html: code}}></script>
            </head>
            <body>
                <div id="root"></div>
            </body>
        </html>,
    );
});

export default app;
