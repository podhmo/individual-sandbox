/** @jsxImportSource jsr:@hono/hono@4.6.14/jsx */
import { type Context, Hono } from "jsr:@hono/hono@4.6.14";
import type { FC } from "jsr:@hono/hono@4.6.14/jsx";

// deno serve --allow-net --port 8080 02index.tsx

const Layout: FC = (props) => {
    return (
        <html>
            <head>
                <title>{props.title}</title>
                <meta charSet="utf-8" />
                <meta
                    name="viewport"
                    content="width=device-width, initial-scale=1"
                />
            </head>
            <body>{props.children}</body>
        </html>
    );
};

const Top: FC<{ messages: string[] }> = (props: {
    messages: string[];
}) => {
    return (
        <Layout title={"hello world"}>
            <h1>Hello, World!</h1>
            <ul>
                {props.messages.map((message) => <li>{message}</li>)}
            </ul>
        </Layout>
    );
};

const app = new Hono();
app.get("/", (c: Context) => {
    const messages = ["Good morning!", "Good afternoon!", "Good evening!"];
    return c.html(<Top messages={messages} />);
});

export default app;
