/** @jsxRuntime automatic */
/** @jsxImportSource npm:react@18 */
/** @jsxImportSourceTypes npm:@types/react@18 */

import {StrictMode} from "npm:react@18";
import {createRoot} from "npm:react-dom@18/client";

const root = createRoot(document.getElementById("app"));
root.render(<StrictMode><h1>hello world</h1></StrictMode>);