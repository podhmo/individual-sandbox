## deno fresh

- https://deno.com/blog/fresh-1.1
- https://fresh.deno.dev/docs/getting-started/

### 作業

init (scaffold)

```console
$ deno run -A -r https://fresh.deno.dev my-app
# deno run -A -r https://fresh.deno.dev/update .
```

開発用のサーバーを立ち上げるのはdeno taskあたりかな（これはdeno.jsonを見れば良さそう）

```console
$ deno task start
```

あとは着々と routes 以下をいじっていく

### vscode

my-app を作った結果、vscode/settings.json の設定を以下にする必要があった。

```json
{
  "deno.enable": true,
  "deno.unstable": true,
  "deno.config": "my-app/deno.json",
  "deno.importMap": "my-app/import_map.json"
}
```

### info

- twind用のvscodeのextension https://github.com/tw-in-js/vscode-twind-intellisense
- preactのdevtool https://chrome.google.com/webstore/detail/preact-developer-tools/ilcajpmogmhpliinlbcdebhbcanbghmd

### plugin

- defaultで生成されたコードではtwindPluginが使われているので、直接classにtwindのcssが使えるっぽい
  - 普通は `import { tw } from "twind"` とかしないとだめ
- 

### 後で調べたいこと

fresh自体のこと

- pluginsの定義方法
- ドキュメントでは型がついていないctxなどに型を付けたい

それ以外のこと

- `import "preact"` と `import "@preact"` の違い (あと `import "$fresh"`)
- index.tsxでclassNameとか使われていないけれど、preactではうまくいくの？
- tailwind(twind)
- preact用のstorybook
