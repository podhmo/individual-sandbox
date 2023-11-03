import { h, Fragment } from 'preact';
import { useEffect, useState, useCallback } from 'preact/hooks';
import { memo } from 'preact/compat';
import type { ComponentChildren } from 'preact';


export const App = () => {
    return (<>
        <h1>hello</h1>
        <Accordion></Accordion>
    </>);
}

async function fetchData() {
    const id = Math.random();
    console.log("start %o", id);
    await new Promise((resolve) => {
        console.log("load %o", id);
        setTimeout(() => { resolve(null); }, 500);
    })
    console.log("end %o", id);
    return ["foo", "bar", "boo"];
}

function Accordion() {
    const [name, setName] = useState("");
    const [items, setItems] = useState([]);

    const changeName = useCallback((e) => {
        setName(e.target.value)
    }, [name]);

    useEffect(async () => {
        console.log(items);
        setItems(await fetchData());
    }, [])

    return (<>
        <input type="text" onChange={changeName}>{name}</input>
        <details role="list">
            <summary role="button">アコーディオン {name}</summary>
            <ul>{items.map((x) => <li key={x}>{x}</li>)}</ul>
        </details>
    </>);
}

