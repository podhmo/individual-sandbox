import { h, Fragment } from 'preact';
import { useState, useCallback } from 'preact/hooks'; // なんかuseStateが壊れているので
// import { useReducer } from "./hooks.js";
import type { ComponentChildren } from 'preact';


export const App = () => {
    // const [finished, updateFinished] = useReducer(() => true, false)
    // const onClick = () => {
    //     updateFinished()
    // }
    // if (finished) return <div>送信しました！</div>
    // return (
    //     <div>
    //         <button onClick={onClick}>送信する</button>
    //     </div>
    // )

    const [value, setValue] = useState(0);
    const increment = useCallback(() => {
        setValue(value + 1);
    }, [value]);

    return (<>
        <h1>hello</h1>
        <details role="list">
            <summary role="button">アコーディオン</summary>
            <ul>{["foo", "bar", "boo"].map((x) => <li>{x}{value}</li>)}</ul>
        </details>
    </>);
}

// export const App = () => {
//   const [value, setValue] = useState(0);
//   const increment = useCallback(() => {
//     setValue(value + 1);
//   }, [value]);
//   return /* @__PURE__ */ h(Fragment, null, /* @__PURE__ */ h("h1", null, "hello"), /* @__PURE__ */ h("details", { role: "list" }, /* @__PURE__ */ h("summary", { role: "button" }, "\u30A2\u30B3\u30FC\u30C7\u30A3\u30AA\u30F3"), /* @__PURE__ */ h("ul", null, ["foo", "bar", "boo"].map((x) => /* @__PURE__ */ h("li", null, x, value)))));
// };
