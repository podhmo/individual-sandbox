# jsoncファイルの読み込み

そういえば、`deno init` で生成されるものが `deno.json` ではなく `deno.jsonc` であって欲しいような気がした。
ところで、jsoncの読み込みにjsonを経由するのが正しいんだろうか？

例えば[`@std/jsonc`](https://jsr.io/@std/jsonc)のparse()で手に入るのはAST的なものなので以下の様な感じで書く。

```ts
const rawConfig = jsonc.parse(await Deno.readTextFile(args.config));
const config = JSON.parse(JSON.stringify(rawConfig)) as Config;
```

直接読み込めると嬉しかったりはした。

