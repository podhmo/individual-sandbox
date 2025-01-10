/** @jsxRuntime automatic */
/** @jsxImportSource npm:react@19 */
/** @jsxImportSourceTypes npm:@types/react@19 */

import { StrictMode, useEffect, useState } from "npm:react@19";
import { createRoot } from "npm:react-dom@19/client";
/// <ts-types path="npm:@types/react-markdown@9"
import ReactMarkdown from "npm:react-markdown@9";

const readmeURL =
  "https://gist.githubusercontent.com/podhmo/395689b310af88566f1df31ed218592d/raw/51ccbb4aeeb6dc4b15cad8b401a38030e014791f/README.md";

function App() {
  const [readme, setReadme] =useState<string | null>(null);
useEffect(() => {
    fetch(readmeURL).then((res) => res.text()).then(setReadme);
  }, []);
  return (
    <>
      <ReactMarkdown>
        {readme}
      </ReactMarkdown>
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
