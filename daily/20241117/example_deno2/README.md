# ts-morphでテキトーな変換を作る

自分のHTML/CSSの学習用の環境を作りたい

## やりたいこと

やりたいことを整理する

- deno用のimport pathをHTMLで使えるように変更する `jsr:<>` -> `<>`
- HTML用のimport mapを生成する `<>` -> `https://esm.sh/jsr/<>`
- それ用のjsを生成してscriptに埋め込む

htmlに一括でbundleされるようなものがほしい？

## 使えそうなもの

- ts-morph (typescriptのcompiler APIのwrapperらしい)
- `deno_*` denoの内部のrust clate (deno_graphなどを使うと楽感はある)
- ↑をwasmに変換したものをtsから使う (deno/emitなどはこの方向)

雑にchatGPTに聞いた。とりあえずts-morphを使った例を試してみる。

https://chatgpt.com/share/6739d60d-3c94-8001-a6e7-3b3c5a29cd68
