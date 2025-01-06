/** @jsxRuntime automatic */
/** @jsxImportSource npm:react@19 */
/** @jsxImportSourceTypes npm:@types/react@19 */

import { StrictMode } from "npm:react@19";
import { createRoot } from "npm:react-dom@19/client";

import { atom, Provider, useAtom } from "npm:jotai";

const counter = atom(0);

const textAtom = atom("hello");
const textLenAtom = atom((get) => get(textAtom).length);
const uppercaseAtom = atom((get) => get(textAtom).toUpperCase());

const Input = () => {
  const [text, setText] = useAtom(textAtom);
  return <input value={text} onChange={(e) => setText(e.target.value)} />;
};

const CharCount = () => {
  const [len] = useAtom(textLenAtom);
  return <div>Length: {len}</div>;
};

const Uppercase = () => {
  const [uppercase] = useAtom(uppercaseAtom);
  return <div>Uppercase: {uppercase}</div>;
};

export default function Page() {
  const [count, setCounter] = useAtom(counter);
  const onClick = () => setCounter((prev) => prev + 1);
  return (
    <div>
      <h1>{count}</h1>
      <button onClick={onClick}>Click</button>
    </div>
  );
}
function App() {
  return (
    <>
      <section>
        <h1>Counter</h1>
        <Page />
      </section>
      <section>
        <h1>Text Length</h1>
        <Provider>
          <Input />
          <CharCount />
          <Uppercase />
        </Provider>
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
