import { h, Fragment } from "preact";
import { useState, useCallback } from "preact/hooks";
const STATE = {
  input: {
    username: "github-notifications",
    query: "",
    debug: false,
    participating: true
  },
  apikey: ""
};
const DEBUG = false;
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
    console.log("submit ev:%o", ev);
    setLoading(true);
    setVersion((prev) => prev + 1);
    setTimeout(() => {
      setLoading(false);
    }, 1e3);
  }, []);
  return /* @__PURE__ */ h(Fragment, null, /* @__PURE__ */ h("h1", { class: "title" }, "GitHub Notifications"), /* @__PURE__ */ h(InputFormPanel, { onSubmit: handleSubmit, loading }), /* @__PURE__ */ h("p", null, /* @__PURE__ */ h("a", { href: "https://github.com/settings/tokens", target: "_blank" }, "please set PAT(personal access token)")), /* @__PURE__ */ h(RawOutputPanel, { input: STATE.input, version, errorMessage }));
}
export function InputFormPanel({ onSubmit, loading }) {
  const [username, setusername] = useState(STATE.input.username);
  const [apikey, setapikey] = useState(STATE.apikey);
  const [query, setquery] = useState(STATE.input.query);
  const [participating, setparticipating] = useState(STATE.input.participating);
  const [debug, setdebug] = useState(STATE.input.debug);
  const params = { username, apikey, query, participating, debug };
  return /* @__PURE__ */ h(Fragment, null, DEBUG && /* @__PURE__ */ h("pre", null, "input: ", JSON.stringify(params, null, 2)), DEBUG && /* @__PURE__ */ h("pre", null, "state: ", JSON.stringify(STATE.input, null, 2)), /* @__PURE__ */ h("form", { method: "POST", id: "auth-form", onSubmit }, /* @__PURE__ */ h("details", { open: true }, /* @__PURE__ */ h("summary", { role: "button", class: "secondary" }, "form"), /* @__PURE__ */ h("div", { style: { paddingLeft: "2rem" } }, /* @__PURE__ */ h("label", { htmlFor: "username" }, "username"), /* @__PURE__ */ h(
    "input",
    {
      type: "text",
      id: "username",
      autoComplete: "username",
      tabIndex: -1,
      onInput: (ev) => setusername((prev) => {
        const v = ev.currentTarget.value;
        STATE.input.username = v;
        return v;
      }),
      value: username
    }
  ), /* @__PURE__ */ h("label", { htmlFor: "password" }, "apikey"), /* @__PURE__ */ h(
    "input",
    {
      type: "password",
      id: "apikey",
      autoComplete: "current-password",
      onInput: (ev) => setapikey((prev) => {
        const v = ev.currentTarget.value;
        STATE.apikey = v;
        return v;
      }),
      value: apikey,
      tabIndex: -1
    }
  ), /* @__PURE__ */ h("label", { htmlFor: "query" }, "query"), /* @__PURE__ */ h(
    "input",
    {
      type: "search",
      id: "query",
      tabIndex: -1,
      onInput: (ev) => setquery((prev) => {
        const v = ev.currentTarget.value;
        STATE.input.query = v;
        return v;
      }),
      value: query
    }
  ), /* @__PURE__ */ h("div", { class: "grid" }, /* @__PURE__ */ h("fieldset", null, /* @__PURE__ */ h("legend", null, "participating"), /* @__PURE__ */ h("label", { htmlFor: "participating" }, /* @__PURE__ */ h(
    "input",
    {
      type: "checkbox",
      id: "participating",
      checked: participating,
      onClick: (ev) => setparticipating((prev) => {
        const v = ev.currentTarget.checked;
        STATE.input.participating = v;
        return v;
      }),
      role: "switch"
    }
  ))), /* @__PURE__ */ h("fieldset", null, /* @__PURE__ */ h("legend", null, "debug"), /* @__PURE__ */ h("label", { htmlFor: "debugStatus" }, /* @__PURE__ */ h(
    "input",
    {
      type: "checkbox",
      id: "debugStatus",
      checked: debug,
      onClick: (ev) => setdebug((prev) => {
        const v = ev.currentTarget.checked;
        STATE.input.debug = v;
        return v;
      }),
      role: "switch"
    }
  )))))), /* @__PURE__ */ h("button", { type: "submit", tabIndex: -1, "aria-busy": loading ? "true" : "false" }, "fetch")));
}
export function RawOutputPanel({ input, version, errorMessage }) {
  const style = { padding: "1rem" };
  if (errorMessage !== "") {
    return /* @__PURE__ */ h("details", { open: true }, /* @__PURE__ */ h("summary", null, " raw response"), /* @__PURE__ */ h("pre", { id: "output", style: { ...style, "background-color": "#fee" } }, errorMessage));
  }
  return /* @__PURE__ */ h("details", { open: true }, /* @__PURE__ */ h("summary", null, " raw response"), /* @__PURE__ */ h("pre", { id: "output", style }, "version", version, ": ", JSON.stringify(input, null, 2)));
}
