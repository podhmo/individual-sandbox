/** @jsx h */
import { render, h, Fragment, ComponentChildren } from "npm:preact";
export { render, h, Fragment };

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