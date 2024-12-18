/** @jsxImportSource npm:react@19 */
/** @jsxImportSourceTypes npm:@types/react@19 */

import { renderToString } from "npm:react-dom@19/server";

// deno run 04main.tsx
const hello = <h1>hello</h1>;
console.log(renderToString(hello));
Deno.exit(0);