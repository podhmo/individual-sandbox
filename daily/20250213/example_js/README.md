# chatgptの内容をmdでクリップボードにコピーしたい

拡張などの更新日は2年前とかで微妙だった。


とりあえずdevtoolsで実行してコピペできるようになってほしい。

https://gist.github.com/podhmo/269f9960cc2131cf2f0c4c51beb418fe#file-chatgpt-to-markdown-js

## devtoolsでclipboard.writeText()が動かない

```js
await navigator.clipboard.writeText("xxxx")
```

以下のようなエラーが出る。

```
Uncaught NotAllowedError: Failed to execute 'writeText' on 'Clipboard': Document is not focused.
    at <anonymous>:1:27
```

どうやらfocusがあたってないとだめらしい。(devtools側にフォーカスがあたっているので上手くいかない)
`F12 ESC > Rendering タブ` でemulate a focusing pageの設定を有効にすると機能するようになる。


> Emulate a focusing page

- https://zenn.dev/lollipop_onl/articles/eoz-devtools-rendering-panel#emulate-a-focusing-page


