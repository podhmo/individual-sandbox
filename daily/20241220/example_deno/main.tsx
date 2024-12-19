import { StrictMode } from "npm:react";
import { createRoot } from "npm:react-dom/client";

const App = () => {
    return (
        <>
            <h1>Hello, world!</h1>
        </>
    );
};

const root = createRoot(document.getElementById("app"));
root.render(
    <StrictMode>
        <App />
    </StrictMode>,
);
