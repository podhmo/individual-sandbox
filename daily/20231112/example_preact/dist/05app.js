import { h, Fragment } from "preact";
import { useState, useReducer, useCallback } from "preact/hooks";
const initialState = {
  errorMessage: "",
  username: "github-notifications",
  apikey: "",
  query: "",
  participating: true,
  debug: false
};
function reducer(state, action) {
  console.log("reducer: ", action.type, JSON.stringify(state, null, 0));
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
  const onError = useCallback((err) => {
    dispatch({ type: "SET_ERROR_MESSAGE", payload: `err: ${err}

${err.stack}` });
  }, []);
  const handleSubmit = useCallback((ev) => {
    ev.preventDefault();
    setLoading(() => true);
    setVersion((prev) => prev + 1);
    setTimeout(() => {
      setLoading(() => false);
    }, 1e3);
  }, []);
  return /* @__PURE__ */ h(Fragment, null, /* @__PURE__ */ h("h1", { class: "title" }, "GitHub Notifications"), /* @__PURE__ */ h(
    InputFormPanel,
    {
      state,
      dispatch,
      onSubmit: handleSubmit,
      loading
    }
  ), /* @__PURE__ */ h("p", null, /* @__PURE__ */ h("a", { href: "https://github.com/settings/tokens", target: "_blank" }, "please set PAT(personal access token)")), /* @__PURE__ */ h(
    RawOutputPanel,
    {
      input: { version, username: state.username, query: state.query, participating: state.participating, debug: state.debug },
      version,
      errorMessage: state.errorMessage
    }
  ));
}
export function InputFormPanel({
  state,
  dispatch,
  loading,
  onSubmit
}) {
  const handleUsernameChange = useCallback(
    (ev) => {
      dispatch({ type: "SET_USERNAME", payload: ev.currentTarget.value });
    },
    []
  );
  const handleApikeyChange = useCallback(
    (ev) => {
      dispatch({ type: "SET_APIKEY", payload: ev.currentTarget.value });
    },
    []
  );
  const handleQueryChange = useCallback(
    (ev) => {
      dispatch({ type: "SET_QUERY", payload: ev.currentTarget.value });
    },
    []
  );
  const handleParticipatingChange = useCallback(
    (ev) => {
      dispatch({ type: "SET_PARTICIPATING", payload: ev.currentTarget.checked });
    },
    []
  );
  const handleDebugChange = useCallback(
    (ev) => {
      dispatch({ type: "SET_DEBUG", payload: ev.currentTarget.checked });
    },
    []
  );
  return /* @__PURE__ */ h("form", { onSubmit }, /* @__PURE__ */ h(
    "input",
    {
      type: "text",
      id: "username",
      placeholder: "Enter username",
      onInput: handleUsernameChange,
      value: state.username
    }
  ), /* @__PURE__ */ h(
    "input",
    {
      type: "text",
      id: "apikey",
      placeholder: "Enter API key",
      onInput: handleApikeyChange,
      value: state.apikey
    }
  ), /* @__PURE__ */ h(
    "input",
    {
      type: "text",
      id: "query",
      placeholder: "Enter query",
      onInput: handleQueryChange,
      value: state.query
    }
  ), /* @__PURE__ */ h("div", { class: "grid" }, /* @__PURE__ */ h("fieldset", null, /* @__PURE__ */ h("legend", null, "participating"), /* @__PURE__ */ h("label", { htmlFor: "participating" }, /* @__PURE__ */ h(
    "input",
    {
      type: "checkbox",
      id: "participating",
      checked: state.participating,
      onClick: handleParticipatingChange,
      role: "switch"
    }
  ))), /* @__PURE__ */ h("fieldset", null, /* @__PURE__ */ h("legend", null, "debug"), /* @__PURE__ */ h("label", { htmlFor: "debugStatus" }, /* @__PURE__ */ h(
    "input",
    {
      type: "checkbox",
      id: "debugStatus",
      checked: state.debug,
      onClick: handleDebugChange,
      role: "switch"
    }
  )))), /* @__PURE__ */ h("button", { type: "submit", tabIndex: -1, "aria-busy": loading ? "true" : "false" }, "fetch"));
}
export function RawOutputPanel({
  input,
  version,
  errorMessage
}) {
  const style = { padding: "1rem" };
  if (errorMessage !== "") {
    return /* @__PURE__ */ h("details", { open: true }, /* @__PURE__ */ h("summary", null, " raw response"), /* @__PURE__ */ h("pre", { id: "output", style: { ...style, "background-color": "#fee" } }, errorMessage));
  }
  return /* @__PURE__ */ h("details", { open: true }, /* @__PURE__ */ h("summary", null, " raw response"), /* @__PURE__ */ h("pre", { id: "output", style }, "version", version, ": ", JSON.stringify(input, null, 2)));
}
