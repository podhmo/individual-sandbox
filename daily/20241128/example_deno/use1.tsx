/** @jsxImportSource ./mini-jsx */
/** @jsxRuntime automatic */

const element = (
    <section className="container">
        <h1>Hello, World!</h1>
        <>
            This is a fragment!
        </>
    </section>
)

console.dir(element, { depth: null })
