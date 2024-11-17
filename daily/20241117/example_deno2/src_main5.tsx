/** @jsx h */
import { h, Fragment, ComponentChildren } from "npm:preact";


export function Code({ children, language }: {
    children: ComponentChildren;
    language?: string;
}) {
    return (
        <pre {...(language ? { language } : {})}>{children}</pre>
    );
}

export const Hello = ({ name }: { name: string }) => {
    return <h1>Hello {name}!!</h1>;
}