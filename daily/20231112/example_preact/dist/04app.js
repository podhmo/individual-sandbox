import { h, Fragment } from "preact";
import { useState, useCallback } from "preact/hooks";
import { signal } from "@preact/signals";
const state = {
  input: {
    username: signal("github-notifications"),
    query: signal(""),
    participating: signal(true),
    debug: signal(false)
  },
  apikey: signal("")
};
export function App() {
  const [version, setVersion] = useState(1);
  const [loading, setLoading] = useState(false);
  const [errorMessage, setErrorMessage] = useState("");
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
      loading
    }
  ), /* @__PURE__ */ h("p", null, /* @__PURE__ */ h("a", { href: "https://github.com/settings/tokens", target: "_blank" }, "please set PAT(personal access token)")), /* @__PURE__ */ h(
    RawOutputPanel,
    {
      input: state.input,
      version,
      errorMessage
    }
  ));
}
export function InputFormPanel({
  onSubmit,
  loading
}) {
  const input = state.input;
  const handleUsernameChange = useCallback(
    (ev) => {
      if (ev.currentTarget) {
        input.username.value = ev.currentTarget.value;
      }
    },
    []
  );
  const handleApikeyChange = useCallback(
    (ev) => {
      if (ev.currentTarget) {
        state.apikey.value = ev.currentTarget.value;
      }
    },
    []
  );
  const handleQueryChange = useCallback(
    (ev) => {
      if (ev.currentTarget) {
        input.query.value = ev.currentTarget.value;
      }
    },
    []
  );
  const handleParticipatingChange = useCallback(
    (ev) => {
      if (ev.currentTarget) {
        input.participating.value = ev.currentTarget.checked;
      }
    },
    []
  );
  const handleDebugChange = useCallback(
    (ev) => {
      if (ev.currentTarget) {
        input.debug.value = ev.currentTarget.checked;
      }
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
      value: input.username
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
      value: input.query
    }
  ), /* @__PURE__ */ h("div", { class: "grid" }, /* @__PURE__ */ h("fieldset", null, /* @__PURE__ */ h("legend", null, "participating"), /* @__PURE__ */ h("label", { htmlFor: "participating" }, /* @__PURE__ */ h(
    "input",
    {
      type: "checkbox",
      id: "participating",
      checked: input.participating,
      onClick: handleParticipatingChange,
      role: "switch"
    }
  ))), /* @__PURE__ */ h("fieldset", null, /* @__PURE__ */ h("legend", null, "debug"), /* @__PURE__ */ h("label", { htmlFor: "debugStatus" }, /* @__PURE__ */ h(
    "input",
    {
      type: "checkbox",
      id: "debugStatus",
      checked: input.debug,
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
