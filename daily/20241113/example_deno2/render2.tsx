import React from "npm:react";
import { JSX as _JSX } from "npm:@types/react";
import { jsx as _jsx } from "npm:react/jsx-runtime";
import { renderToString } from "npm:react-dom/server";
import Markdown from "npm:react-markdown@9.0.1";

import { parseArgs } from "jsr:@podhmo/with-help@0.4.0";

function App({ children }: { children: React.ReactNode }): React.ReactElement {
    return (
        <html lang="ja" data-theme="dark">
            <head>
                <title></title>
                <meta charSet="utf-8"></meta>
                <meta name="viewport" content="width=device-width, initial-scale=1.0"></meta>
                <meta name="color-scheme" content="light dark"></meta>
                <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css" />
            </head>
            <body>
                <main className="container">
                    {children}
                </main>
            </body>
        </html>
    );
}

// deno run --allow-read --allow-env render2.tsx --md README.md
function main() {
    const args = parseArgs(Deno.args, {
        description: "render HTML from markdown",
        string: ["md"],
        required: ["md"]
    });

    const text = Deno.readTextFileSync(args.md);

    const html = `<!DOCTYPE html>${renderToString(
        <App>
            <article>
                <Markdown>{text}</Markdown>
            </article>
        </App>
    )}`;
    console.log(html);
}

if (import.meta.main) {
    main();
}

