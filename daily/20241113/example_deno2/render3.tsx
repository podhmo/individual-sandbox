import React from "npm:react";
import { JSX as _JSX } from "npm:@types/react";
import { jsx as _jsx } from "npm:react/jsx-runtime";
import { renderToString } from "npm:react-dom/server";
import Markdown from "npm:react-markdown@9.0.1";
import remarkGfm from "npm:remark-gfm@4.0.0";
import { PrismLight as SyntaxHighlighter } from "npm:react-syntax-highlighter@15.6.1";
import ts from "npm:react-syntax-highlighter@15.6.1/dist/esm/languages/prism/typescript.js";
import js from "npm:react-syntax-highlighter@15.6.1/dist/esm/languages/prism/javascript.js";
import dark from "npm:react-syntax-highlighter@15.6.1/dist/esm/styles/prism/dark.js";
import { parseArgs } from "jsr:@podhmo/with-help@0.4.0";

SyntaxHighlighter.registerLanguage("typescript", ts);
SyntaxHighlighter.registerLanguage("ts", ts);
SyntaxHighlighter.registerLanguage("javascript", js);
SyntaxHighlighter.registerLanguage("js", js);

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
                <Markdown remarkPlugins={[remarkGfm]} components={{
                    code(props) {
                        const { children, className, node, ...rest } = props;
                        const match = /language-(\w+)/.exec(className || "");
                        
                        console.error(`match=${match} className=${className} node=${node?.tagName} rest=${JSON.stringify(rest)}`);
                        return match ? (
                            <SyntaxHighlighter style={dark} language={match[1]} PreTag="div" children={String(children).replace(/\n$/, "")} {...rest} />
                        ) : (
                            <code className={className} {...rest}>{children}</code>
                        )
                    }
                }}>
                    {text}
                </Markdown>
            </article>
        </App>
    )}`;
    console.log(html);
}

if (import.meta.main) {
    main();
}

