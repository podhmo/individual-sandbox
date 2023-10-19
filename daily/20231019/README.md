# esbuild

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

## with index.html (server)

TODO

https://esbuild.github.io/api/#serve
