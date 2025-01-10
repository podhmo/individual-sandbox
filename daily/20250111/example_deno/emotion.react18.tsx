/** @jsxRuntime automatic */
/** @jsxImportSource npm:react@18 */
/** @jsxImportSourceTypes npm:@types/react@18 */

import { StrictMode } from "npm:react@18";
import { createRoot } from "npm:react-dom@18/client";

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
