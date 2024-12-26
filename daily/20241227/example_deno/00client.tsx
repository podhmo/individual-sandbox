/** @jsxRuntime automatic */
/** @jsxImportSource npm:preact */
/** @jsxImportSourceType npm:@types/preact */

import { render } from "npm:preact";

function App() {
    return <p class="big">Hello World!</p>;
}

// main
const root = document.getElementById("app");
if (root !== null) {
    for (const child of Array.from(root.children)) {
        root.removeChild(child);
    }
    render(<App />, root);
}
