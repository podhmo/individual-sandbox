import { h, Fragment } from 'preact';
import { useState, useCallback } from 'preact/hooks';
import type { ComponentChildren } from 'preact';


const STATE = {
    input: {
        username: "github-notifications",
        query: "",
        debug: false,
        participating: true,
    },
    apikey: ""
}

const DEBUG = false;
type todofixSubmitHandler = any

export function App() {
    const [version, setVersion] = useState<number>(1);

    const [loading, setLoading] = useState<boolean>(false);
    const [errorMessage, setErrorMessage] = useState<string>("");
    const onError = useCallback((err: Error) => {
        setErrorMessage(() => `err: ${err}\n\n${err.stack}`);
    }, [])

    const handleSubmit = useCallback((ev: SubmitEvent) => {
        ev.preventDefault()
        console.log("submit ev:%o", ev);
        setLoading(true);
        setVersion((prev) => prev + 1)
        setTimeout(() => {
            setLoading(false);
        }, 1000);
    }, [])


    return (
        <>
            <h1 class="title">GitHub Notifications</h1>
            <InputFormPanel onSubmit={handleSubmit} loading={loading}></InputFormPanel>
            <p><a href="https://github.com/settings/tokens" target="_blank">please set PAT(personal access token)</a></p>

            <RawOutputPanel input={STATE.input} version={version} errorMessage={errorMessage}></RawOutputPanel>
        </>
    );
}

export function InputFormPanel({ onSubmit, loading }: { onSubmit: todofixSubmitHandler; loading: boolean }) {
    const [username, setusername] = useState<string>(STATE.input.username);
    const [apikey, setapikey] = useState<string>(STATE.apikey);
    const [query, setquery] = useState<string>(STATE.input.query);
    const [participating, setparticipating] = useState<boolean>(STATE.input.participating);
    const [debug, setdebug] = useState<boolean>(STATE.input.debug); // todo: rename to verbose

    const params = { username, apikey, query, participating, debug };
    return (<>
        {DEBUG && <pre>input: {JSON.stringify(params, null, 2)}</pre>}
        {DEBUG && <pre>state: {JSON.stringify(STATE.input, null, 2)}</pre>}
        <form method="POST" id="auth-form" onSubmit={onSubmit}>
            <details open>
                <summary role="button" class="secondary">form</summary>
                <div style={{ paddingLeft: "2rem" }}>
                    <label htmlFor="username">username</label>
                    <input
                        type="text"
                        id="username"
                        autoComplete="username"
                        tabIndex={-1}
                        onInput={(ev) => setusername((prev) => { const v = ev.currentTarget.value; STATE.input.username = v; return v })}
                        value={username}
                    />
                    <label htmlFor="password">apikey</label>
                    <input
                        type="password"
                        id="apikey"
                        autoComplete="current-password"
                        onInput={(ev) => setapikey((prev) => { const v = ev.currentTarget.value; STATE.apikey = v; return v })}
                        value={apikey}
                        tabIndex={-1}
                    />
                    <label htmlFor="query">query</label>
                    <input
                        type="search"
                        id="query"
                        tabIndex={-1}
                        onInput={(ev) => setquery((prev) => { const v = ev.currentTarget.value; STATE.input.query = v; return v })}
                        value={query}
                    />
                    <div class="grid">
                        <fieldset>
                            <legend>participating</legend>
                            <label htmlFor="participating">
                                <input
                                    type="checkbox"
                                    id="participating"
                                    checked={participating}
                                    onClick={(ev) => setparticipating((prev) => { const v = ev.currentTarget.checked; STATE.input.participating = v; return v })}
                                    role="switch"
                                />
                            </label>
                        </fieldset>
                        <fieldset>
                            <legend>debug</legend>
                            <label htmlFor="debugStatus">
                                <input
                                    type="checkbox"
                                    id="debugStatus"
                                    checked={debug}
                                    onClick={(ev) => setdebug((prev) => { const v = ev.currentTarget.checked; STATE.input.debug = v; return v })}
                                    role="switch" />
                            </label>
                        </fieldset>
                    </div>
                </div>
            </details>
            <button type="submit" tabIndex={-1} aria-busy={loading ? "true" : "false"}>
                fetch
            </button>
        </form>
    </>
    )
}

export function RawOutputPanel({ input, version, errorMessage }: { input?: Record<string, string | boolean>; version: number; errorMessage?: string }) {
    const style = { padding: "1rem" };
    if (errorMessage !== "") {
        return (
            <details open>
                <summary> raw response</summary>
                <pre id="output" style={{ ...style, "background-color": "#fee" }}>{errorMessage}</pre>
            </details>
        );
    }

    return (
        <details open>
            <summary> raw response</summary>
            <pre id="output" style={style}>version{version}: {JSON.stringify(input, null, 2)}</pre>
        </details>
    );
}