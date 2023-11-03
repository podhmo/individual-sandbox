import { h, Fragment } from 'preact';
import { useEffect, useState, useCallback } from 'preact/hooks';
import { memo } from 'preact/compat';
import type { ComponentChildren } from 'preact';


export const App = () => {
    const [count, setCount] = useState(0);
    const [isLoading, setLoading] = useState(false);
    const handleClick = useCallback(() => {
        if (isLoading) {
            return
        }
        console.log("count: %o", count)
        setCount((v) => v + 1)
        setLoading(() => true);
        setTimeout(() => { setLoading(false) }, 1000);
    }, [isLoading])
    return (<>
        <h1>loading animation</h1>

        <ButtonWithLoading title={`load:${count}`} isLoading={isLoading} onClick={handleClick} />
    </>);
}

function ButtonWithLoading({ title, isLoading, onClick }: { title: string; isLoading: boolean; onClick: any }) {
    return (
        <button onclick={onClick} style={{ "position": "relative" }}>
            {title} {isLoading && <span class="loading" style={{ "position": "absolute", "margin-left": "0.5rem" }}></span>}
        </button>
    );
}