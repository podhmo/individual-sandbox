/** @jsxRuntime automatic */
/** @jsxImportSource npm:react@19 */
/** @jsxImportSourceTypes npm:@types/react@19 */

import { StrictMode } from "npm:react@19";
import { createRoot } from "npm:react-dom@19/client";

import Button from "npm:@mui/material@6/Button";

function App() {
  return (
    <>
      <h1>MUI example</h1>
      <a href="https://mui.com/material-ui/getting-started/usage/">https://mui.com/material-ui/getting-started/usage/</a>
      <section>
        <Button variant="contained">Hello World</Button>
      </section>
    </>
  );
}

// main
const root = createRoot(document.getElementById("root"));
root.render(
  <StrictMode>
    <App />
  </StrictMode>,
);
