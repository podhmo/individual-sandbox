/** @jsxRuntime automatic */
/** @jsxImportSource npm:react@19 */
/** @jsxImportSourceTypes npm:@types/react@19 */

import { StrictMode, useState } from "npm:react@19";
import { createRoot } from "npm:react-dom@19/client";

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
      <h1>Counter</h1>
      <Counter />
    </>
  );
}

// main
const root = createRoot(document.getElementById("app"));
root.render(
  <StrictMode>
    <App />
  </StrictMode>,
);
