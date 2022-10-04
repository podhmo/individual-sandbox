## deno html配信

- cli -> std/flags から parse
- print -> console.log
- file io -> await Deno.readTextFile(...)
- http server -> http/server から serve

こんな感じでhtmlを配信することはできる。

```
  const text = await Deno.readTextFile(filename);
  const article = ReactDomServer.renderToString(Article(text));
```

とかやっているのはひどいので、そろそろfreshを把握する


## deno fresh

https://fresh.deno.dev/

