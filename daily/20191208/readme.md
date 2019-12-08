## js 毎回待つの辛い

- https://github.com/mjackson/unpkg
- https://www.pika.dev/about/

unpkgのcache的なものをlocalに立てられないかな。。

pikaはちょっとめんどくさそうだった。

- https://github.com/mjackson/unpkg/issues/98

### hmm

- https://github.com/KevinSheedy/unpkg-server

それでコレにたどり着くのか。。

- https://github.com/zalari/docker-unpkg

### 頑張る

see https://github.com/KevinSheedy/unpkg-server

```console
$ make 
$ xdg-open ./00hello/index.html
```

もうちょっと必要そう。

```
21:46:54.536 Cross-Origin Request Blocked: The Same Origin Policy disallows reading the remote resource at http://localhost:44444/react@16/umd/react.development.js. (Reason: CORS request did not succeed).
```

自分自身越しに実行しないとダメか。上手くexpressをアレコレするのがめんどくさかった。goでテキトーなreverse proxyを書こう。。

時折なんか404が返るようになって以下のようなエラーが出る。

```
22:43:09.262 The resource from “http://localhost:55555/unpkg/babel-standalone@6/babel.min.js” was blocked due to MIME type (“text/plain”) mismatch (X-Content-Type-Options: nosniff). 00hello
```

なんとなく動いた。

うーん。evergreen-ui悪くなさそうだったけれど。umd形式のものが入っていなかった。残念。
(あとでフロントエンドで変換する方法を把握すると良さそう)

## ui 実は今一番欲しい知識はUIなのではないか？

カタログをさらっと試すような。componentもだるいね。。

### evergreen使ってみるか

- https://evergreen.segment.com/get-started/introduction

あとそもそもcomponentの数を知らないな。。

### tableau-ui

この辺試してみる？

- https://github.com/tableau/tableau-ui
- https://tableau.github.io/tableau-ui/docs/index.html

### ui

- tailwind
- https://github.com/segmentio/evergreen
- https://github.com/palantir/blueprint
- https://material-ui.com/components/box/
- https://github.com/palantir/blueprint
