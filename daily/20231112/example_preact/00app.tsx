import { h, Fragment } from 'preact';
import { useState } from 'preact/hooks';
import type { ComponentChildren } from 'preact';


export const App = () => {
    const [count, setCount] = useState(0);

    return (<>
        <h1>hello</h1>
        <button onClick={() => setCount(() => count + 1)}>click me {count}</button>
    </>);
}
