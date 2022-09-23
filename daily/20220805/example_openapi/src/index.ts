import ReactDomServer from "react-dom/server";
import { Article } from "./main.tsx";
import { parse } from "https://deno.land/std@0.157.0/flags/mod.ts";
import { serve } from "https://deno.land/std@0.157.0/http/server.ts";

const { _: [filename], ...options } = parse(Deno.args);
const _ = options; // hmm
const port = 8080; // TODO: use flags

const handler = async (_request: Request): Promise<Response> => {
  const text = await Deno.readTextFile(filename);
  const article = ReactDomServer.renderToString(Article(text));
  const html = `<!DOCTYPE html>
    <html lang="ja">
    <meta charset="UTF-8">
    <title>${filename}</title>
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.1.0/github-markdown.min.css">
    <style>
        .markdown-body {
            box-sizing: border-box;
            min-width: 200px;
            max-width: 980px;
            margin: 0 auto;
            padding: 45px;
        }
        @media (max-width: 767px) {
            .markdown-body {
                padding: 15px;
            }
        }
    </style>
    <body>
    <article class="markdown-body">
    ${article}
    </article>
    </body>
    <html>`;
  return new Response(html, {
    status: 200,
    headers: { "content-type": "text/html" },
  });
};

console.log(`HTTP webserver running. Access it at: http://localhost:8080/`); // TODO: stderr
await serve(handler, { port });
