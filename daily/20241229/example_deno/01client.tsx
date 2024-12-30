/** @jsxRuntime automatic */
/** @jsxImportSource npm:react@18 */
/** @jsxImportSourceTypes npm:@types/react@18 */

import { StrictMode, useState, useEffect } from "npm:react@18";
import { createRoot } from "npm:react-dom@18/client";

function App() {
    const [items, setItems] = useState<string[]>([]);
    useEffect(() => {
        fetch("/api/readme")
            .then((response) => response.json())
            .then(({ items }) => setItems(items));
    }, []);

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
