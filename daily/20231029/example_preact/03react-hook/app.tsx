import React from "react";
import { useState } from 'react';

export const App = () => {
    const [count, setCount] = useState(0);
    return (<>
        <h1>hello</h1>
        <button onClick={() => { setCount(count+1)}}>count:{count}</button>
    </>);
}
