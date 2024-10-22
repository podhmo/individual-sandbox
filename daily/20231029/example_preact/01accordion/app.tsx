import { h, Fragment } from 'preact';
import type { ComponentChildren } from 'preact';

type AccordionProps = {
    title: string;
    children: ComponentChildren;
};

// accordion TODO: childrenを動的に生成したい
function Accordion({ title, children }): AccordionProps {
    return (
        <details open>
            <summary>{title}</summary>
            {children}
        </details>
    );
}

export const App = () => {
    return (<>
    <h1>hello</h1>
    <Accordion title="アコーディオン">
        <ul>
            <li>foo</li>
            <li>bar</li>
            <li>boo</li>
        </ul>
    </Accordion>
    </>);
}