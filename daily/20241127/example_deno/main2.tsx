/** @jsx E */

import { E, render } from "./minijsx.ts";

const Hello = () => <p>Hello</p>;

// main

console.dir(Hello(), { depth: null });
console.log(render(Hello()));

// Output:
// { tag: "p", props: null, children: [ "Hello" ] }
// <p >Hello</p>
