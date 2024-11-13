// @jsx react-jsx
import React from "npm:react";
import Markdown from "npm:react-markdown@9.0.1";
import { renderToString } from "npm:react-dom/server";
import { parseArgs } from "jsr:@podhmo/with-help@0.4.0"


// deno run --allow-read --allow-env render1.tsx --md README.md
const args = parseArgs(Deno.args, {
    description: "render HTML from markdown",
    string: ["md"],
    required: ["md"]
});

const text = Deno.readTextFileSync(args.md);
const vdom = (
    <Markdown>
        {text}
    </Markdown>
);
console.log(renderToString(vdom))
