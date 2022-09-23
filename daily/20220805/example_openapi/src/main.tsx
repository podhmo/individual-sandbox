import React from "react";
import ReactMarkdown from "https://esm.sh/react-markdown@8.0.3"

// https://github.com/remarkjs/react-markdown

export const Article = (text:string) => (
    <ReactMarkdown>{text}</ReactMarkdown>
)