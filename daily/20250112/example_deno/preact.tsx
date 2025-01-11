/** @jsxRuntime automatic */
/** @jsxImportSource npm:preact@10 */
/** @jsxImportSourceTypes npm:preact@10 */

import "npm:preact@10/debug";
import { render } from "npm:preact@10";
import { useState } from "npm:preact@10/hooks";

// ----------------------------------------
// components
// ----------------------------------------
function Counter() {
  const [count, setCount] = useState(0);
  const onIncrement = () => setCount((prev: number) => prev + 1);
  const onDecrement = () => setCount((prev: number) => prev - 1);

  return (
    <>
      <p>Count: {count}</p>
      <button onClick={onIncrement}>Increment</button>
      <button onClick={onDecrement}>Decrement</button>
    </>
  );
}

function App() {
  return (
    <>
      <h1>
        <a href="/">top</a> {">"} Counter
      </h1>
      <Counter />
    </>
  );
}

// ----------------------------------------
// main
// ----------------------------------------
const root = document.getElementById("root");
if (root !== null) {
  for (const child of Array.from(root.children)){
    root.removeChild(child);
  }
  render(<App />, root);
}
