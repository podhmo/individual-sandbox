/** @jsxRuntime automatic */
/** @jsxImportSource npm:react@18 */
/** @jsxImportSourceTypes npm:@types/react@18 */

import { StrictMode, useEffect, useState } from "npm:react@18";
import { createRoot } from "npm:react-dom@18/client";
/// <ts-types path="npm:@types/react-markdown@8"
import ReactMarkdown from "npm:react-markdown@8";

const readmeURL =
  "https://gist.githubusercontent.com/podhmo/395689b310af88566f1df31ed218592d/raw/74f9ad1c6897a9d32875338de8734b32383b9aab/README.md";

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
