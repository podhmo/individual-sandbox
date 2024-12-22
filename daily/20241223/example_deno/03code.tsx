/** @jsxRuntime automatic */
/** @jsxImportSource npm:react@18 */
/** @jsxImportSourceTypes npm:@types/react@18 */

import { StrictMode } from "npm:react@18";
import { createRoot } from "npm:react-dom@18/client";
import { useState } from "npm:react@18";
// import { jsx as _jsx } from 'npm:react@19/react/jsx-runtime';

function Counter() {
  const [count, setCount] = useState(0);

  return (
    <div>
      <p>{count}</p>
      <button onClick={() => setCount(count + 1)}>Increment</button>
    </div>
  );
}

export default function App() {
  return (
    <main>
      <h1>hello world</h1>
      <Counter />
    </main>
  );
}

const root = createRoot(document.getElementById("root"));
root.render(
  <StrictMode>
    <App />
  </StrictMode>,
);
