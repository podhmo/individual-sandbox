/** @jsxRuntime automatic */
/** @jsxImportSource npm:react@18 */
/** @jsxImportSourceTypes npm:@types/react@18 */

import { StrictMode } from "npm:react@18";
import { createRoot } from "npm:react-dom@18/client";
/// <ts-types path="npm:@types/react-markdown@8"
import ReactMarkdown from "npm:react-markdown@8";

function App() {
  return (
    <>
      <ReactMarkdown>
        # Hello, *world*!
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
