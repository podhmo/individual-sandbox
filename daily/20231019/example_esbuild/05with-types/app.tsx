import * as React from 'react'
import * as Server from 'react-dom/server'


export function Greet({ name }: { name: string }) {
    return (
        <h1>Hello, name!</h1>
    )
}

console.log(Server.renderToString(
    <React.StrictMode>
        <Greet name="World" />
    </React.StrictMode>
));