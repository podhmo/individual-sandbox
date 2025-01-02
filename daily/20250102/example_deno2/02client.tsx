/** @jsxRuntime automatic */
/** @jsxImportSource npm:react@18 */
/** @jsxImportSourceTypes npm:@types/react@18 */

import { StrictMode, useState } from "npm:react@18";
import { createRoot } from "npm:react-dom@18/client";
// import { BrowserRouter, Link, Route, Routes } from "npm:react-router@7";
import { BrowserRouter, Link, Route, Routes } from "https://esm.sh/react-router@7?deps=react@18,react-dom@18";

// https://reactrouter.com/start/library/routing

function Counter() {
  const [count, setCount] = useState(0);
  const onIncrement = () => setCount((prev: number) => prev + 1);
  const onDecrement = () => setCount((prev: number) => prev - 1);
  return (
    <>
      <h1>Counter</h1>
      <p>Count: {count}</p>
      <button onClick={onIncrement}>Increment</button>
      <button onClick={onDecrement}>Decrement</button>
    </>
  );
}

function Hello() {
  return (
    <>
      <h1>Hello</h1>
      <Link to="/counter">Counter</Link>
    </>
  );
}

// main
const root = createRoot(document.getElementById("app"));
root.render(
  <BrowserRouter>
    <Routes>
      <Route path="/" element={<Hello />} />
      <Route path="/counter" element={<Counter />} />
    </Routes>
  </BrowserRouter>,
);
