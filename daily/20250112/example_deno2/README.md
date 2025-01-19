# denoでもgoのembedのようなことがしたい

雑に相対パスのimportだけをbundleしたesbuildのpluginを書いてみてもテキストファイルなどが取り込まれないので死ぬ。
こういうときにgoのembedが羨ましくなる。

- https://gist.github.com/podhmo/63cbe181f4e56d81fdea4e6f2027f88b

## jsonは行ける

https://docs.deno.com/examples/importing_json/

```ts
import jsondata from "./data.json" with { type: "json" };

console.log("jsondata:\n%o", jsondata);
```

## textやblobでimportしたい..

どうやら無理そう。jsonしか対応していないみたい。
もちろんesbuildのpluginを使ってbundle時にあれこれみたいなことをやればどうにかなる話ではあるのだけれど...

テンプレートのようなものを扱うscaffoldのようなものをbundleしたい場合にこういう機能があるとべんりだった。残念。