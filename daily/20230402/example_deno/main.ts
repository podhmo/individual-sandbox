import { denoDomDisableQuerySelectorCodeGeneration, DOMParser, Element } from "deno_dom/deno-dom-wasm.ts";
import { parse } from "https://deno.land/std/flags/mod.ts";

function walk(node: Element, indent: number) {
  const row = ["  ".repeat(indent), node.tagName.toLowerCase()]
  if (node.id !== "") {
    row.push("#")
    row.push(node.id)
  }
  if (node.className !== "") {
    row.push(".")
    row.push(node.className)
  }
  console.log(row.join(""));
  for (const el of Array.from(node.children)) {
    walk(el as Element, indent + 1);
  }
}

type E = {
  element: string
  className?: string
  children?: E[]
}

function transform(node: Element, indent: number): E {
  const target: E = { "element": node.tagName }
  if (node.className !== "") {
    target.className = node.className
  }

  if (node.children.length > 0) {
    const children = [];
    for (const el of Array.from(node.children)) {
      children.push(transform(el as Element, indent + 1));
    }
    target.children = children
  }
  return target
}


if (import.meta.main) {
  const flags = parse(Deno.args, {
    boolean: ["help", "color"],
    string: ["filename"],
    default: { color: true },
  });

  if (flags.help) {
    console.log("<> --filename=<html>")
    Deno.exit(1)
  }

  const filename: string = flags.filename!
  const decoder = new TextDecoder("utf-8")
  const data = await Deno.readFile(filename);
  const document = new DOMParser().parseFromString(decoder.decode(data), "text/html");

  const el = document?.querySelector("body")!
  walk(el, 0);

  console.log("----------------------------------------")
  console.log(JSON.stringify(transform(el, 0), null, "  "))
}
