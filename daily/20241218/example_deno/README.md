# honoを使って手軽にapi serverなどを立ち上げる

昨日の03の内容からroutingでhonoを使った方が楽そうという感じだったのでhonoを使ってみる

- https://gist.github.com/podhmo/a209e873077b67ac16dff5d619839462

jsr.ioにもhonoがあるらしい（ラッパーライブラリを使うときにはnpmのものを使うことをおすすめされてはいる）。

- https://docs.deno.com/examples/hono/
- https://hono.dev/docs/getting-started/deno

## 00 hello world

とりあえずapiを使ってecho serverを作る

```console
$ http -b :8080
{
    "message": "Hello, World!"
}
```

## 01 path変数を使う

path変数は`:id`みたいな感じの表現で設定するみたい

- https://hono.dev/docs/getting-started/basic#request-and-response
- https://hono.dev/docs/api/routing

どうやらcontext経由で触るらしい

- path変数は`c.req.param(name)`
- query stringは`c.req.query`
- bodyは？`await c.req.json()` ?

詳細はこの辺を見れば良さそう。

- https://hono.dev/docs/api/context
- https://hono.dev/docs/api/request

```console
$ http -b :8080/hello/deno
{
    "message": "Hello, deno!"
}
```

## 02 jsxを使う

今度は雑にindex pageをjsxで記述する



## references

- https://docs.deno.com/examples/hono/
- https://hono.dev/docs/getting-started/deno
- https://hono.dev/docs/getting-started/basic#request-and-response
- https://hono.dev/docs/api/routing
- https://hono.dev/docs/api/context
- https://hono.dev/docs/api/request