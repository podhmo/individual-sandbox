/** @jsxRuntime automatic */
/** @jsxImportSource npm:react@18 */
/** @jsxImportSourceTypes npm:@types/react@18 */

import { StrictMode } from "npm:react@18";
import { createRoot } from "npm:react-dom@18/client";

function App() {
    return (
        <section>
            <h1>hello world</h1>
            <p>HMRがほしい理由がわかってきた</p>
        </section>
    );
}

const root = createRoot(document.getElementById("app"));
root.render(
    <StrictMode>
        <App></App>
    </StrictMode>,
);

