/** @jsxRuntime automatic */
/** @jsxImportSource npm:react@18 */
/** @jsxImportSourceTypes npm:@types/react@18 */

import { StrictMode } from "npm:react@18";
import { createRoot } from "npm:react-dom@18/client";

async function App() {
    const response = await fetch("/api/readme");
    if (!response.ok) {
        return (
            <section>
                <h1>hello world</h1>
                <p>"/readme" {response.statusText}</p>
            </section>
        );
    }

    const { items } = await response.json();
    return (
        <section>
            <h1>hello world</h1>
            <p>HMRがほしい理由がわかってきた</p>
            <ul>
                {items.map((item: string) => <li key={item}>{item}</li>)}
            </ul>
        </section>
    );
}

const root = createRoot(document.getElementById("app"));
root.render(
    <StrictMode>
        <App></App>
    </StrictMode>,
);
