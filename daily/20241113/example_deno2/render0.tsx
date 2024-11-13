// @jsx react-jsx
import React from "npm:react";
import Markdown from "npm:react-markdown@9.0.1";
import { renderToString } from "npm:react-dom/server";

const text = `
# Hello

This is a paragraph.

- This is a list item
- This is another list item
- This is a third list item

## This is a subheading

This is another paragraph.**Bold text** and *italic text*.

\`\`\`js
console.log("Hello, world!");
\`\`\`

This is a [link](https://example.com).

| Header 1 | Header 2 |
|----------|----------|
| Cell 1   | Cell 2   |
| Cell 3   | Cell 4   |

`;
const vdom = (
    <Markdown>
        {text}
    </Markdown>
);
console.log(renderToString(vdom))
