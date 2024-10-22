import { h, Fragment } from 'preact';
import { useState, useReducer, useCallback } from 'preact/hooks';
import type { ComponentChildren } from 'preact';
import type { JSX } from "preact";


type Action =
    | { type: "SET_ERROR_MESSAGE"; payload: string }
    | { type: "SET_USERNAME"; payload: string }
    | { type: "SET_APIKEY"; payload: string }
    | { type: "SET_QUERY"; payload: string }
    | { type: "SET_PARTICIPATING"; payload: boolean }
    | { type: "SET_DEBUG"; payload: boolean };

const initialState = {
    errorMessage: "",
    username: "github-notifications",
    apikey: "",
    query: "",
    participating: true,
    debug: false,
};
type State = typeof initialState;

function reducer(state: State, action: Action): State {
    console.log("reducer: ", action.type, JSON.stringify(state, null, 0))
    switch (action.type) {
        case "SET_ERROR_MESSAGE":
            return { ...state, errorMessage: action.payload };
        case "SET_USERNAME":
            return { ...state, username: action.payload };
        case "SET_APIKEY":
            return { ...state, apikey: action.payload };
        case "SET_QUERY":
            return { ...state, query: action.payload };
        case "SET_PARTICIPATING":
            return { ...state, participating: action.payload };
        case "SET_DEBUG":
            return { ...state, debug: action.payload };
        default:
            return state;
    }
}

export function App() {
    const [state, dispatch] = useReducer(reducer, initialState);

    const [loading, setLoading] = useState(false);
    const [version, setVersion] = useState(0);

    const onError = useCallback((err: Error) => {
        dispatch({ type: "SET_ERROR_MESSAGE", payload: `err: ${err}\n\n${err.stack}` });
    }, []);

    const handleSubmit = useCallback((ev: JSX.TargetedEvent<HTMLFormElement>) => {
        ev.preventDefault();
        setLoading(() => true);
        setVersion((prev) => prev + 1);
        setTimeout(() => {
            setLoading(() => false);
        }, 1000);
    }, []);

    return (
        <>
            <h1 class="title">GitHub Notifications</h1>
            <InputFormPanel
                state={state}
                dispatch={dispatch}
                onSubmit={handleSubmit}
                loading={loading}
            ></InputFormPanel>
            <p>
                <a href="https://github.com/settings/tokens" target="_blank">
                    please set PAT(personal access token)
                </a>
            </p>

            <RawOutputPanel
                input={{ version: version, username: state.username, query: state.query, participating: state.participating, debug: state.debug }}
                version={version}
                errorMessage={state.errorMessage}
            ></RawOutputPanel>
        </>
    );
}

type InputFormPanelProps = {
    state: State;
    dispatch: (action: Action) => void;

    loading: boolean;
    onSubmit: (ev: JSX.TargetedEvent<HTMLFormElement>) => void;
};


export function InputFormPanel({
    state, dispatch,
    loading,
    onSubmit,
}: InputFormPanelProps) {
    const handleUsernameChange = useCallback(
        (ev: JSX.TargetedEvent<HTMLInputElement>) => {
            dispatch({ type: "SET_USERNAME", payload: ev.currentTarget.value });
        },
        []
    );

    const handleApikeyChange = useCallback(
        (ev: JSX.TargetedEvent<HTMLInputElement>) => {
            dispatch({ type: "SET_APIKEY", payload: ev.currentTarget.value });
        },
        []
    );

    const handleQueryChange = useCallback(
        (ev: JSX.TargetedEvent<HTMLInputElement>) => {
            dispatch({ type: "SET_QUERY", payload: ev.currentTarget.value });
        },
        []
    );

    const handleParticipatingChange = useCallback(
        (ev: JSX.TargetedEvent<HTMLInputElement>) => {
            dispatch({ type: "SET_PARTICIPATING", payload: ev.currentTarget.checked });
        },
        []
    );

    const handleDebugChange = useCallback(
        (ev: JSX.TargetedEvent<HTMLInputElement>) => {
            dispatch({ type: "SET_DEBUG", payload: ev.currentTarget.checked });
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
                value={state.username}
            />
            <input
                type="text"
                id="apikey"
                placeholder="Enter API key"
                onInput={handleApikeyChange}
                value={state.apikey}
            />
            <input
                type="text"
                id="query"
                placeholder="Enter query"
                onInput={handleQueryChange}
                value={state.query}
            />
            <div class="grid">
                <fieldset>
                    <legend>participating</legend>
                    <label htmlFor="participating">
                        <input
                            type="checkbox"
                            id="participating"
                            checked={state.participating}
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
                            checked={state.debug}
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
    input: Record<string, string | number | boolean>;
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
