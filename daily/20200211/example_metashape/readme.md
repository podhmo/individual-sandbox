# いろいろな形状への変換

- fn -> object
- object -> fn

個別に実装したせいか、逆の処理なのに書き方が異なるなー。
fnspecはそれ単体での文字列化に関心があってmetashapeの方は情報の取得に関心がある。
それはそれとしてalias周りを良い感じに扱いたい。

あと何があるだろう？

## others

- JSONからTypedDict
- JSONからdataclasses

## hmm

大きいデータの形状が分かりづらい
大きいデータから間引くのが難しい
フラットに展開されても状況がわからない。

## mitmproxy?

- (.harからopenAPI)
- (任意のrequest/responseから.harを生成)

## 型からjsonschemaに対する思い

型からjsonschemaで満足ができないのは、その実装そのものではなく、その実装を作るためのツールキットにこそ価値があるからだと思っている気がする。
そしてその実装をすること自体が通常のコーディングになってほしい。
