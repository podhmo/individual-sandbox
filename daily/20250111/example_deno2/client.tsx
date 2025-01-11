/** @jsxRuntime automatic */
/** @jsxImportSource npm:react@19 */
/** @jsxImportSourceTypes npm:@types/react@19 */

import "npm:twind/shim";
import { StrictMode } from "npm:react@19";
import { createRoot } from "npm:react-dom@19/client";
import Home from "./app__page.tsx";

// ----------------------------------------
// main
// ----------------------------------------
const root = createRoot(document.getElementById("root"));
root.render(
  <StrictMode>
    <Home />
  </StrictMode>,
);
