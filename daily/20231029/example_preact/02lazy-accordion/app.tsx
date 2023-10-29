import { h, Fragment } from 'preact';
import { useState, useCallback } from 'preact/hooks';
import { memo } from 'preact/compat';
import type { ComponentChildren } from 'preact';


export const App = () => {
    return (<>
        <h1>hello</h1>
        <Accordion></Accordion>
    </>);
}


function Accordion() {
    const [items, setitems] = useState([]);
    const lazyItems = useCallback(() => {
        console.log("called")
        setitems(["foo", "bar", "boo"]);
    }, [items]);

    return (<>
        <details role="list" onClick={lazyItems}>
            <summary role="button">アコーディオン</summary>
            <ul>{items.map((x) => <li>{x}</li>)}</ul>
        </details>
    </>);
}

