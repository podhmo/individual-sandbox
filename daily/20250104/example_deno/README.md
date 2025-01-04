# esm.shでreact@19を動かしたい

以前雑に実行したときにはdevelopment modeじゃないと動かなかった。
今回はdeno.lockを利用して明示的に依存関係を列挙する感じでリクエストするようになったので動くはず。

## 00 init

initをするとreact@18用のものが出力される。

```console
$ deno run -A jsr:@podhmo/glue@0.2.2 init
$ ls 
app.ts client.tsx
```

## 01 react@19に上げる

19にあげたあとに、deno.lockを生成しておく

```console
$ sed -i "s/@18/@19/g" *.tsx
$ echo "{}" > deno.json
$ deno cache -r *.tsx
```

実際に動かしてみる。`--debug` を付けてる。
(deno.lockが見つからないと `[WARN] deno.lock is not found` 的なメッセージが表示される)


今回は動いていそうだった。 `import { createRoot } from "/react-dom@19.0.0/client?deps=react@19.0.0,scheduler@0.25.0";` というような感じでdepsを追加しているのでまともに動いている感じそう。


実行ログ。

<details>

```console
$ deno run -A jsr:@podhmo/glue@0.2.2 serve --debug --port 8080 app.ts
[DEBUG] guessed deno.json is /home/po/ghq/github.com/podhmo/individual-sandbox/daily/20250104/example_deno/deno.json
Listening on http://127.0.0.1:8080/
[DEBUG] load deno.json from /home/po/ghq/github.com/podhmo/individual-sandbox/daily/20250104/example_deno/deno.json
[DEBUG] load deno.lock from /home/po/ghq/github.com/podhmo/individual-sandbox/daily/20250104/example_deno/deno.json
[DEBUG] setup resolve npm:@types/react@19 -> /@types/react@19.0.2
[DEBUG] setup resolve npm:react-dom@19 -> /react-dom@19.0.0
[DEBUG] setup resolve npm:react@19 -> /react@19.0.0
[DEBUG] setup resolve jsr:*: -> /jsr/*
[DEBUG] setup resolve npm:* -> /*
[DEBUG] rewrite npm:react@19 -> /react@19.0.0
[DEBUG] rewrite npm:react-dom@19/client -> /react-dom@19.0.0/client?deps=react@19.0.0,scheduler@0.25.0
[DEBUG] rewrite npm:react@19/jsx-runtime -> /react@19.0.0/jsx-runtime
proxy request[200]: https://esm.sh/react@19.0.0
proxy request[200]: https://esm.sh/react-dom@19.0.0/client?deps=react%4019.0.0%2Cscheduler%400.25.0
proxy request[200]: https://esm.sh/react@19.0.0/jsx-runtime
proxy request[200]: https://esm.sh/stable/react@19.0.0/es2022/jsx-runtime.js
proxy request[200]: https://esm.sh/v135/scheduler@0.25.0/es2022/scheduler.mjs
proxy request[200]: https://esm.sh/v135/react-dom@19.0.0/es2022/react-dom.mjs
proxy request[200]: https://esm.sh/v135/react-dom@19.0.0/X-ZC9zY2hlZHVsZXJAMC4yNS4w/es2022/client.js
proxy request[200]: https://esm.sh/stable/react@19.0.0/es2022/react.mjs
proxy request[200]: https://esm.sh/v135/node_process.js
proxy request[200]: https://esm.sh/v135/node_events.js
```

</details>

以下のようなresponseを返している。

```console
$ http -b :8080
<!DOCTYPE html>
<html data-theme="dark">
<head>
<meta charset="utf-8" />
<title>Counter</title>
<meta name="viewport" content="width=device-width, initial-scale=1" />
<meta name="color-scheme" content="light dark" />
<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css" />
</head>
<body>
<main id=app class="container">
<h1>...</h1>
</main>
<script type="module">
// client.tsx
import { StrictMode, useState } from "/react@19.0.0";
import { createRoot } from "/react-dom@19.0.0/client?deps=react@19.0.0,scheduler@0.25.0";
import { Fragment, jsx, jsxs } from "/react@19.0.0/jsx-runtime";
function Counter() {
  const [count, setCount] = useState(0);
  const onIncrement = () => setCount((prev) => prev + 1);
  const onDecrement = () => setCount((prev) => prev - 1);
  return /* @__PURE__ */ jsxs(Fragment, { children: [
    /* @__PURE__ */ jsxs("p", { children: [
      "Count: ",
      count
    ] }),
    /* @__PURE__ */ jsx("button", { onClick: onIncrement, children: "Increment" }),
    /* @__PURE__ */ jsx("button", { onClick: onDecrement, children: "Decrement" })
  ] });
}
function App() {
  return /* @__PURE__ */ jsxs(Fragment, { children: [
    /* @__PURE__ */ jsx("h1", { children: "Counter" }),
    /* @__PURE__ */ jsx(Counter, {})
  ] });
}
var root = createRoot(document.getElementById("app"));
root.render(
  /* @__PURE__ */ jsx(StrictMode, { children: /* @__PURE__ */ jsx(App, {}) })
);

</script>
</body>
</html>
```
