# react用のライブラリがesm.shで動くかを試す。

## 00 init

とりあえず以下でcounterが動くことを確認する

```console
$ TSX=./00client.tsx deno run -A jsr:@podhmo/glue@0.2.1 serve --port 8080 ./main.ts
```

## 01 React Router

テキトーに見て使ってみる。ファイルを分けずに使えるんだろうか？

- https://reactrouter.com/start/library/installation
- https://reactrouter.com/start/library/routing

なんか怒られてた

- https://react.dev/warnings/invalid-hook-call-warning
- https://react.dev/reference/react/Component#catching-rendering-errors-with-an-error-boundary

reactのバージョンを19にあげたら上手く動く様になった。辛い。これはesm.shでリクエストするときに上手く固定できずに依存を取ってきている感じになっていそう？ 

react-router@7がreact@19に依存してそう？ つらい。

出力

<details>

react@18

```
proxy request[302]: https://esm.sh/react-dom@18/client?dev=
redirect: /react-dom@18.3.1/client?dev=
proxy request[302]: https://esm.sh/react-router@7?dev=
redirect: /react-router@7.1.1?dev=
proxy request[302]: https://esm.sh/react@18/jsx-runtime?dev=
redirect: /react@18.3.1/jsx-runtime?dev=
proxy request[200]: https://esm.sh/react-dom@18.3.1/client?dev=
proxy request[200]: https://esm.sh/react-router@7.1.1?dev=
proxy request[200]: https://esm.sh/react@18.3.1/jsx-runtime?dev=
proxy request[200]: https://esm.sh/v135/react-dom@18.3.1/es2022/react-dom.development.mjs
proxy request[200]: https://esm.sh/v135/react-dom@18.3.1/es2022/client.development.js
proxy request[200]: https://esm.sh/stable/react@19.0.0/es2022/react.development.mjs
proxy request[200]: https://esm.sh/v135/turbo-stream@2.4.0/es2022/turbo-stream.development.mjs
proxy request[200]: https://esm.sh/v135/cookie@1.0.2/es2022/cookie.development.mjs
proxy request[200]: https://esm.sh/v135/set-cookie-parser@2.7.1/es2022/set-cookie-parser.development.mjs
proxy request[200]: https://esm.sh/v135/react-router@7.1.1/es2022/react-router.development.mjs
proxy request[200]: https://esm.sh/stable/react@18.3.1/es2022/react.development.mjs
proxy request[200]: https://esm.sh/stable/react@18.3.1/es2022/jsx-runtime.development.js
proxy request[200]: https://esm.sh/v135/scheduler@0.23.2/es2022/scheduler.development.mjs
proxy request[200]: https://esm.sh/stable/react@19.0.0/es2022/react.development.mjs.map
proxy request[200]: https://esm.sh/v135/react-dom@18.3.1/es2022/react-dom.development.mjs.map
proxy request[200]: https://esm.sh/v135/react-router@7.1.1/es2022/react-router.development.mjs.map
proxy request[200]: https://esm.sh/v135/react-dom@18.3.1/es2022/client.development.js.map
proxy request[200]: https://esm.sh/v135/scheduler@0.23.2/es2022/scheduler.development.mjs.map
proxy request[200]: https://esm.sh/stable/react@18.3.1/es2022/react.development.mjs.map
proxy request[200]: https://esm.sh/v135/cookie@1.0.2/es2022/cookie.development.mjs.map
proxy request[200]: https://esm.sh/v135/turbo-stream@2.4.0/es2022/turbo-stream.development.mjs.map
proxy request[200]: https://esm.sh/stable/react@18.3.1/es2022/jsx-runtime.development.js.map
proxy request[200]: https://esm.sh/v135/set-cookie-parser@2.7.1/es2022/set-cookie-parser.development.mjs.map
```

react@19 

```
proxy request[302]: https://esm.sh/react-router@7?dev=
redirect: /react-router@7.1.1?dev=
proxy request[200]: https://esm.sh/react-router@7.1.1?dev=
proxy request[200]: https://esm.sh/v135/node_process.js
proxy request[200]: https://esm.sh/v135/node_events.js
proxy request[302]: https://esm.sh/react@19/jsx-runtime?dev=
redirect: /react@19.0.0/jsx-runtime?dev=
proxy request[302]: https://esm.sh/react-dom@19/client?dev=
redirect: /react-dom@19.0.0/client?dev=
proxy request[200]: https://esm.sh/react-dom@19.0.0/client?dev=
proxy request[200]: https://esm.sh/v135/react-dom@19.0.0/es2022/react-dom.development.mjs
proxy request[200]: https://esm.sh/v135/scheduler@0.25.0/es2022/scheduler.development.mjs
proxy request[200]: https://esm.sh/v135/react-dom@19.0.0/es2022/client.development.js
proxy request[200]: https://esm.sh/react@19.0.0/jsx-runtime?dev=
proxy request[200]: https://esm.sh/stable/react@19.0.0/es2022/jsx-runtime.development.js
proxy request[200]: https://esm.sh/v135/react-dom@19.0.0/es2022/react-dom.development.mjs.map
proxy request[200]: https://esm.sh/v135/react-dom@19.0.0/es2022/client.development.js.map
proxy request[200]: https://esm.sh/v135/scheduler@0.25.0/es2022/scheduler.development.mjs.map
proxy request[200]: https://esm.sh/stable/react@19.0.0/es2022/jsx-runtime.development.js.map
```

</details>

とりあえずは動いたけれど、ブラウザでリロードするとesm.shにリダイレクトされてしまうかも？
あとreact@19はなぜかdevelopment modeじゃないとesm.shで配信されるものでは動かない。


## 02 依存関係を細かく指定してみる 

https://esm.sh/#specifiying-dependencies 

`?deps=react@18` とか指定してあげる必要がある？

```diff
--- 01client.tsx	2025-01-02 21:37:54.146854620 +0900
+++ 02client.tsx	2025-01-02 21:55:41.178540223 +0900
@@ -1,10 +1,11 @@
 /** @jsxRuntime automatic */
-/** @jsxImportSource npm:react@19 */
-/** @jsxImportSourceTypes npm:@types/react@19 */
+/** @jsxImportSource npm:react@18 */
+/** @jsxImportSourceTypes npm:@types/react@18 */
 
-import { StrictMode, useState } from "npm:react@19";
-import { createRoot } from "npm:react-dom@19/client";
-import { BrowserRouter, Link, Route, Routes } from "npm:react-router@7";
+import { StrictMode, useState } from "npm:react@18";
+import { createRoot } from "npm:react-dom@18/client";
+// import { BrowserRouter, Link, Route, Routes } from "npm:react-router@7";
+import { BrowserRouter, Link, Route, Routes } from "https://esm.sh/react-router@7?deps=react@18,react-dom@18";
 
 // https://reactrouter.com/start/library/routing
 ```

やってみたがだめっぽい。なんか依存関係でエラーが出ることがあるらしい。闇っぽい。深入りは辞めておこう。
たぶん別のシングルトン的なものにアクセスしててnullが返ってきたり対応していないものにアクセスしてnullが返ってきてたりする？（バージョン違いか別のシングルトンにアクセス）

```
21:58:50.712 react-dom.production.min.js:188 TypeError: Cannot read properties of null (reading 'useRef')
    at n.useRef (react.production.min.js:26:177)
```

(out of context: ) 関係ないけれどどうやら`--development`の処理が雑だったみたい。複数のquery stringに対応してなさそう。

## 03 TanStack Router

TODO:

- https://tanstack.com/router/latest/docs/framework/react/start/overview

