# denoでdev serverを作ってHTMLの練習をする

以前の成果をもとにhtmlのreloadを行うdev serverを作りpico.cssを使ったHTMLの練習をしたい。

- https://gist.github.com/podhmo/d942b56968c768853bd0983d5c2d84fd#file-02server-with-watch-ts

dev serverの仕様

- htmlをSSEのhandling付きで返すhtml server
- SSEで"reload"を取り込むとreloadする

## 00 html serverを作成

`deno init --serve` から始める。
色々いじった結果、特に必要にならなかったので全部書き換えた。

とりあえずHTMLファイルを配信するようなコードを書く。

## 01 SSEのhandlingを追加する

これは雑にjsを"</body>"に埋め込むだけで良い気がする。
それっぽく実行した。

どうやら初回作成時にreloadされてしまうと困るみたい。
lstatとかで空だったら中止するか。

## 02 なんとなく表示されるものをいじっていきたい

