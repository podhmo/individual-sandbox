# esbuild

- code [./example_esbuild](./example_esbuild)

## WSL esbuild

めんどくさいのでbuildする

```
git clone --depth 1 --branch v0.19.4 https://github.com/evanw/esbuild.git
cd esbuild
go checkout v0.19.4
go install -v ./cmd/esbuild
```

まぁpluginとかはつかえないのだけど。
https://esbuild.github.io/getting-started/#build-from-source

## 00 app.jsx

とりあえず、app.jsxを何らかのファイルに変換されれば良い。

## 01 sourcemap

色々オプションがあるらしいがとりあえず生成できるか確認するだけ

https://esbuild.github.io/api/#source-maps

## preact

tsconfig.jsonでfactoryを変えれば良いだけ？

https://esbuild.github.io/api/#jsx-factory

TODO: importが異なるかも?

## with index.html (python -m http.server)

https://esm.sh を使ってimportしてあげる必要がある
その上でtype=moduleでjsを読み込む必要があるのでhttp serverを立ち上げなければいけない。

## with index.html (esbuild)

https://esbuild.github.io/api/#serve

esbuildの `--server` オプションで配信もできるみたい。

```console
$ esbuild app.jsx --outfile=out.js --serve --servedir=.
```

esm.shの話は同様に置きうるけれど、import mapにブラウザが対応しているみたいなのでこれを使えば良さそう

- https://github.com/WICG/import-maps#the-import-map 
- https://caniuse.com/import-maps

## 型が欲しい

ここでnpmが必要になってしまう？ `npm install --save-dev @types/react @types/react-dom`
denoでも良いといえば良い。

- まともな人 はnpmを使う (esbuildも./node_modules/.bin/esbuild)
- おかしな人 esbuildとdenoで済ませる
- deno好きな人 全部denoで済ませる

そういえば、bunはどうなんだろう?

## 素直にviteを使う

```console
# react, typescriptを選ぶ
$ npm create vite@latest 10use-vite
```
