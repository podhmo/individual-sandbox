import { h, Fragment } from 'preact';
import { useState, useCallback } from 'preact/hooks';
import { signal } from '@preact/signals'
import type { ComponentChildren } from 'preact';
import type { JSX } from "preact";

// state
const username = signal<string>("github-notifications");
const apikey = signal<string>("");
const query = signal<string>("");
const participating = signal<boolean>(true);
const debug = signal<boolean>(false);


export function App() {
    const [version, setVersion] = useState<number>(1);

    const [loading, setLoading] = useState<boolean>(false);
    const [errorMessage, setErrorMessage] = useState<string>("");

    const onError = useCallback((err: Error) => {
        setErrorMessage(() => `err: ${err}\n\n${err.stack}`);
    }, []);

    const handleSubmit = useCallback((ev: JSX.TargetedEvent<HTMLFormElement>) => {
        ev.preventDefault();
        setLoading(true);
        setVersion((prev) => prev + 1);
        setTimeout(() => {
            setLoading(false);
        }, 1000);
    }, []);

    return (
        <>
            <h1 class="title">GitHub Notifications</h1>
            <InputFormPanel
                onSubmit={handleSubmit}
                loading={loading}
            ></InputFormPanel>
            <p>
                <a href="https://github.com/settings/tokens" target="_blank">
                    please set PAT(personal access token)
                </a>
            </p>

            <RawOutputPanel
                input={{
                    username: username.value,
                    query: query.value,
                    participating: participating.value,
                    debug: debug.value,
                }}
                version={version}
                errorMessage={errorMessage}
            ></RawOutputPanel>
        </>
    );
}

export function InputFormPanel({
    onSubmit,
    loading,
}: {
    onSubmit: (ev: JSX.TargetedEvent<HTMLFormElement>) => void,
    loading: boolean;
}) {
    const handleUsernameChange = useCallback(
        (ev: JSX.TargetedEvent<HTMLInputElement>) => {
            if (ev.currentTarget) { username.value = ev.currentTarget.value; }
        },
        []
    );

    const handleApikeyChange = useCallback(
        (ev: JSX.TargetedEvent<HTMLInputElement>) => {
            if (ev.currentTarget) {apikey.value = ev.currentTarget.value; }
        },
        []
    );

    const handleQueryChange = useCallback(
        (ev: JSX.TargetedEvent<HTMLInputElement>) => {
            if (ev.currentTarget) {query.value = ev.currentTarget.value; }
        },
        []
    );

    const handleParticipatingChange = useCallback(
        (ev: JSX.TargetedEvent<HTMLInputElement>) => {
            if (ev.currentTarget) {participating.value = ev.currentTarget.checked; }
        },
        []
    );

    const handleDebugChange = useCallback(
        (ev: JSX.TargetedEvent<HTMLInputElement>) => {
            if (ev.currentTarget) {debug.value = ev.currentTarget.checked; }
        },
        []
    );

    return (
        <form onSubmit={onSubmit}>
            <input
                type="text"
                id="username"
                placeholder="Enter username"
                onInput={handleUsernameChange}
                value={username}
            />
            <input
                type="text"
                id="apikey"
                placeholder="Enter API key"
                onInput={handleApikeyChange}
                value={apikey}
            />
            <input
                type="text"
                id="query"
                placeholder="Enter query"
                onInput={handleQueryChange}
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
                            onClick={handleParticipatingChange}
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
                            onClick={handleDebugChange}
                            role="switch"
                        />
                    </label>
                </fieldset>
            </div>
            <button type="submit" tabIndex={-1} aria-busy={loading ? "true" : "false"}>
                fetch
            </button>
        </form>
    );
}

export function RawOutputPanel({
    input,
    version,
    errorMessage,
}: {
    input: Record<string, string | boolean>;
    version: number;
    errorMessage: string;
}) {
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
