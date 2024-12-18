/** @jsxRuntime automatic */
// for esbuild

/** @jsxImportSource jsr:@hono/hono@4.6.14/jsx/dom */
/** @jsxImportSourceTypes jsr:@hono/hono@4.6.14/jsx */
// for type-checking

import { useState } from "jsr:@hono/hono@4.6.14/jsx";

export const Top = (props: { name?: string }) => {
    const [name, setName] = useState(props.name || "world");
    console.log(name);
    return (
        <>
            <h1>hello {name}</h1>
            <input value={name} onChange={(e)=> setName(e.target.value)}></input>
            <h2>state</h2>
            <pre>name: {name}</pre>
        </>
    );
};
