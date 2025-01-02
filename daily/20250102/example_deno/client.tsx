/** @jsxRuntime automatic */
/** @jsxImportSource npm:react@18 */
/** @jsxImportSourceTypes npm:@types/react@18 */

import { StrictMode, useEffect, useState } from "npm:react@18";
import { createRoot } from "npm:react-dom@18/client";

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

function Readme() {
  const [readme, setReadme] = useState("");
  useEffect(() => {
    fetch("/readme")
      .then((res) => res.json())
      .then((json) => setReadme(json.text));
  }, []);
  return (
    <>
      <pre>{readme}</pre>;
    </>
  );
}

function App() {
  return (
    <>
      <section>
        <h2>Counter</h2>
        <Counter />
      </section>
      <section>
        <h2>Readme</h2>
        <Readme />
      </section>
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
