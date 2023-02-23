import React from "https://esm.sh/react@18.2.0?bundle";
import ReactMarkdown from "https://esm.sh/react-markdown@8.0.5?bundle";
import remarkGfm from "https://esm.sh/remark-gfm@3.0.1?bundle";
import { createRoot } from "https://esm.sh/react-dom@18.2.0/client?bundle";
import { Prism as SyntaxHighlighter } from "https://esm.sh/react-syntax-highlighter@15.5.0?bundle";

const flatten = (text, child) => {
  return typeof child === "string"
    ? text + child
    : React.Children.toArray(child.props.children).reduce(flatten, text);
};

const HeadingRenderer = (props) => {
  const children = React.Children.toArray(props.children);
  const text = children.reduce(flatten, "");
  const slug = text.toLowerCase().replace(/[{\/\.}]+/g, "").replace(
    /[ \t]+/g,
    "-",
  );

  const a = React.createElement("a", {
    "class": "x-anchor",
    "aria-hidden": "true",
    href: "#" + slug,
  }, props.children);
  return React.createElement("h" + props.level, {
    id: slug,
    tabindex: "-1",
    dir: "auto",
  }, a);
};

const languageRegex = /language-(\w+)/;

const SyntaxHighlightRenderer = (
    { node, inline, className, children, ...props },
  ) => {
    const match = languageRegex.exec(className || "");
    return !inline && match
      ? SyntaxHighlighter(
        {
          language: match[1],
          PreTag: "div",
          children: children,
          ...props,
        },
      )
      : React.createElement("code", { className: className, ...props }, children);
  };

  const SyntaxHighlightRendererForPre = (props) => {
    const child = props.children[0].props;
	const match = languageRegex.exec(child.className || "");
	return !props.inline && match ? SyntaxHighlightRenderer({...child}) : React.createElement("pre", {}, props.children);
  };
    
const text = document.getElementById("mdtext").innerText;

const domNode = document.getElementById("mdbody");
const root = createRoot(domNode);
root.render(
  ReactMarkdown({
    children: text,
    components: {
      "h1": HeadingRenderer,
      "h2": HeadingRenderer,
      "h3": HeadingRenderer,
      "pre": SyntaxHighlightRendererForPre,
    },
    remarkPlugins: [remarkGfm],
  }),
);
