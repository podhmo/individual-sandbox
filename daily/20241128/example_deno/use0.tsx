/** @jsxImportSource ./mini-jsx */

import { JSX } from "./mini-jsx/jsx-runtime.ts"

const element = (
    <section className="container">
        <h1>Hello, World!</h1>
        <>
            This is a fragment!
        </>
    </section>
)

console.dir(element, { depth: null })
