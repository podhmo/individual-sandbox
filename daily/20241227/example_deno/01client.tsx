/** @jsxRuntime automatic */
/** @jsxImportSource npm:preact@10.25.3 */
/** @jsxImportSourceType npm:@types/preact */

import { render } from "npm:preact@10.25.3";
import { signal } from "npm:@preact/signals@2.0.0";

// Create a signal that can be subscribed to
const count = signal(0);

function Counter() {
    // Accessing .value in a component will cause the component to re-render
    const value = count.value;

    const increment = () => {
        console.log("increment", count.value);
        count.value++;
    };
    const decrement = () => count.value--;

    return (
        <div>
            <p>Count: {count}</p>
            <button onClick={increment}>Increment {value}</button>
            <button onClick={decrement}>Decrement {value}</button>
        </div>
    );
}

function App() {
    return (
        <>
            <h1>Counter</h1>
            <Counter />
        </>
    );
}

// main
const root = document.getElementById("app");
if (root !== null) {
    for (const child of Array.from(root.children)) {
        root.removeChild(child);
    }
    render(<App />, root);
}
