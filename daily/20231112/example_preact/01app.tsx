import { h, Fragment } from 'preact';
import { useState } from 'preact/hooks';
import { signal } from '@preact/signals';
import type { ComponentChildren } from 'preact';


const count = signal(0);

export const App = () => {
    return (<>
        <h1>hello</h1>
        <button onClick={() => count.value++}>click me {count.value}</button>
    </>);
}
