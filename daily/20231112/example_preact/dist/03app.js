import { h, Fragment } from "preact";
import { useState, useCallback } from "preact/hooks";
export function App() {
  const [version, setVersion] = useState(1);
  const [loading, setLoading] = useState(false);
  const [errorMessage, setErrorMessage] = useState("");
  const [username, setUsername] = useState("github-notifications");
  const [apikey, setApikey] = useState("");
  const [query, setQuery] = useState("");
  const [participating, setParticipating] = useState(true);
  const [debug, setDebug] = useState(false);
  const onError = useCallback((err) => {
    setErrorMessage(() => `err: ${err}

${err.stack}`);
  }, []);
  const handleSubmit = useCallback((ev) => {
    ev.preventDefault();
    setLoading(true);
    setVersion((prev) => prev + 1);
    setTimeout(() => {
      setLoading(false);
    }, 1e3);
  }, []);
  return /* @__PURE__ */ h(Fragment, null, /* @__PURE__ */ h("h1", { class: "title" }, "GitHub Notifications"), /* @__PURE__ */ h(
    InputFormPanel,
    {
      onSubmit: handleSubmit,
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
      setDebug
    }
  ), /* @__PURE__ */ h("p", null, /* @__PURE__ */ h("a", { href: "https://github.com/settings/tokens", target: "_blank" }, "please set PAT(personal access token)")), /* @__PURE__ */ h(
    RawOutputPanel,
    {
      input: {
        username,
        query,
        participating,
        debug
      },
      version,
      errorMessage
    }
  ));
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
  setDebug
}) {
  const handleUsernameChange = useCallback(
    (ev) => {
      ev.currentTarget && setUsername(ev.currentTarget.value);
    },
    [setUsername]
  );
  const handleApikeyChange = useCallback(
    (ev) => {
      ev.currentTarget && setApikey(ev.currentTarget.value);
    },
    [setApikey]
  );
  const handleQueryChange = useCallback(
    (ev) => {
      ev.currentTarget && setQuery(ev.currentTarget.value);
    },
    [setQuery]
  );
  const handleParticipatingChange = useCallback(
    (ev) => {
      ev.currentTarget && setParticipating(ev.currentTarget.checked);
    },
    [setParticipating]
  );
  const handleDebugChange = useCallback(
    (ev) => {
      ev.currentTarget && setDebug(ev.currentTarget.checked);
    },
    [setDebug]
  );
  return /* @__PURE__ */ h("form", { onSubmit }, /* @__PURE__ */ h(
    "input",
    {
      type: "text",
      id: "username",
      placeholder: "Enter username",
      onInput: handleUsernameChange,
      value: username
    }
  ), /* @__PURE__ */ h(
    "input",
    {
      type: "text",
      id: "apikey",
      placeholder: "Enter API key",
      onInput: handleApikeyChange,
      value: apikey
    }
  ), /* @__PURE__ */ h(
    "input",
    {
      type: "text",
      id: "query",
      placeholder: "Enter query",
      onInput: handleQueryChange,
      value: query
    }
  ), /* @__PURE__ */ h("div", { class: "grid" }, /* @__PURE__ */ h("fieldset", null, /* @__PURE__ */ h("legend", null, "participating"), /* @__PURE__ */ h("label", { htmlFor: "participating" }, /* @__PURE__ */ h(
    "input",
    {
      type: "checkbox",
      id: "participating",
      checked: participating,
      onClick: handleParticipatingChange,
      role: "switch"
    }
  ))), /* @__PURE__ */ h("fieldset", null, /* @__PURE__ */ h("legend", null, "debug"), /* @__PURE__ */ h("label", { htmlFor: "debugStatus" }, /* @__PURE__ */ h(
    "input",
    {
      type: "checkbox",
      id: "debugStatus",
      checked: debug,
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
