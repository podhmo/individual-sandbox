// @jsx react
// @jsxFactory React.createElement
// @jsxFrag React.Fragment
import React from "npm:react";
import { renderToString } from "npm:react-dom/server";

function App({ children }: { children: React.ReactNode }): React.ReactElement {
    return (
        <html lang="ja" data-theme="dark">

            <head>
                <title></title>
                <meta charSet="utf-8"></meta>
                <meta name="viewport" content="width=device-width, initial-scale=1.0"></meta>

                {/* style */}
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

if (import.meta.main) {
    const html = `<!DOCTYPE html>${renderToString(
        <App >
            <article>
                <h1>hello world</h1>
                <p>こんにちは 世界</p>
            </article>
        </App>)}`;
    console.log(html);
}
