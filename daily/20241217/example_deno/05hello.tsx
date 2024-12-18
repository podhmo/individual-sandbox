/** @jsxImportSource npm:react@19 */
/** @jsxImportSourceTypes npm:@types/react@19 */

import { renderToString } from "npm:react-dom@19/server";

const Hello = ({ name }: { name: string }) => {
    return (
        <>
            <h1>Hello, {name}!</h1>
            <input value={name} />
            <button>Submit</button>
        </>
    );
};

const Page = (
    props: { title?: string; children: React.ReactNode },
) => {
    const title = props.title ?? "";
    const { children } = props;

    return (
        <html lang="ja">
            <head>
                <meta charSet="utf-8" />
                <meta
                    name="viewport"
                    content="width=device-width, initial-scale=1.0"
                />
                <title>{title}</title>
            </head>
            <body>
                {children}
            </body>
        </html>
    );
};

export function indexPage(_req: Request): Response {
    const jsx = (
        <Page title="Hello Someone">
            <Hello name="Someone" />
        </Page>
    );

    return new Response(
        renderToString(jsx),
        {
            headers: { "content-type": "text/html" },
        },
    );
}

export function helloAPI(_req: Request, name: string | undefined): Response {
    return new Response(`{"message": "Hello, ${name}!"}`, {
        status: 200,
        headers: { "content-type": "application/json" },
    });
}

export default {
    fetch: (req: Request) => {
        const contenType = req.headers.get("content-type");
        if (contenType !== "application/json") {
            return indexPage(req);
        }
        const route = new URLPattern({ pathname: "/api/hello/:name" });

        const match = route.exec(req.url);
        if (match) {
            const name = match.pathname.groups.name;
            return helloAPI(req, name);
        }

        return new Response(`{"error": "Not found"}`, {
            status: 404,
            headers: { "content-type": "application/json" },
        });
    },
};
