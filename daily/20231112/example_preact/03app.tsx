import { h, Fragment } from 'preact';
import { useState, useCallback } from 'preact/hooks';
import type { ComponentChildren } from 'preact';
import type { JSX } from "preact";

export function App() {
    const [version, setVersion] = useState<number>(1);

    const [loading, setLoading] = useState<boolean>(false);
    const [errorMessage, setErrorMessage] = useState<string>("");

    const [username, setUsername] = useState<string>("github-notifications");
    const [apikey, setApikey] = useState<string>("");
    const [query, setQuery] = useState<string>("");
    const [participating, setParticipating] = useState<boolean>(true);
    const [debug, setDebug] = useState<boolean>(false);

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
                username={username}
                setUsername={setUsername}
                apikey={apikey}
                setApikey={setApikey}
                query={query}
                setQuery={setQuery}
                participating={participating}
                setParticipating={setParticipating}
                debug={debug}
                setDebug={setDebug}
            ></InputFormPanel>
            <p>
                <a href="https://github.com/settings/tokens" target="_blank">
                    please set PAT(personal access token)
                </a>
            </p>

            <RawOutputPanel
                input={{
                    username,
                    query,
                    participating,
                    debug,
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
    username,
    setUsername,
    apikey,
    setApikey,
    query,
    setQuery,
    participating,
    setParticipating,
    debug,
    setDebug,
}: {
    onSubmit: (ev: JSX.TargetedEvent<HTMLFormElement>) => void,
    loading: boolean;
    username: string;
    setUsername: (username: string) => void;
    apikey: string;
    setApikey: (apikey: string) => void;
    query: string;
    setQuery: (query: string) => void;
    participating: boolean;
    setParticipating: (participating: boolean) => void;
    debug: boolean;
    setDebug: (debug: boolean) => void;
}) {
    const handleUsernameChange = useCallback(
        (ev: JSX.TargetedEvent<HTMLInputElement>) => {
            ev.currentTarget && setUsername(ev.currentTarget.value);
        },
        [setUsername]
    );

    const handleApikeyChange = useCallback(
        (ev: JSX.TargetedEvent<HTMLInputElement>) => {
            ev.currentTarget && setApikey(ev.currentTarget.value);
        },
        [setApikey]
    );

    const handleQueryChange = useCallback(
        (ev: JSX.TargetedEvent<HTMLInputElement>) => {
            ev.currentTarget && setQuery(ev.currentTarget.value);
        },
        [setQuery]
    );

    const handleParticipatingChange = useCallback(
        (ev: JSX.TargetedEvent<HTMLInputElement>) => {
            ev.currentTarget && setParticipating(ev.currentTarget.checked);
        },
        [setParticipating]
    );

    const handleDebugChange = useCallback(
        (ev: JSX.TargetedEvent<HTMLInputElement>) => {
            ev.currentTarget && setDebug(ev.currentTarget.checked);
        },
        [setDebug]
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
