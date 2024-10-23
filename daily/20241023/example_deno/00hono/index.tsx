import { Hono } from "npm:hono";
import type { FC } from "npm:hono/jsx";

const app = new Hono();

const Layout: FC = (props) => {
    return (
        <html lang="ja">
            <head>
                <meta charSet="utf-8" />
                <title>Hello Hono</title>
            </head>
            <body>{props.children}</body>
        </html>
    );
};

const Top: FC<{ messages: string[] }> = (props: {
    messages: string[];
}) => {
    return (
        <Layout>
            <h1>Hello Hono!</h1>
            <ul>
                {props.messages.map((message) => {
                    return <li key={message}>{message}!!</li>;
                })}
            </ul>
        </Layout>
    );
};

app.get("/", (c) => {
    const messages = ["Good Morning", "Good Evening", "Good Night"];
    return c.html(<Top messages={messages} />);
});

export default app;
