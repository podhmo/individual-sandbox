## go test関係を整理したい感じ

何が必要なんだろう？

あとせっかくだしたくさんrepositoryを作るのが面倒なので昔に作ったrepositoryに相乗りしちゃおうかな。

- https://github.com/podhmo/go-webtest

### 必要なものを整理

- snapshot testing -- 考えてみるとreplayというよりsnapshot
- jsonequal -- 異なるstructだけれどJSONとして同じものを同じとして扱いたい
- transform的な何か -- 自動で発行される部分を良い感じに置き換えたい

jsonequalもsnapshot testingのものもJSONがdefaultの表現というだけなのだけれど。

### snapshot testing

https://github.com/podhmo/go-webtest/snapshot

`snapshot.Take()` という形なのは良い感じかも。

### jsonqeual

https://github.com/podhmo/go-webtest/jsonqeual

`jsonequal.Equal()` でerrorを返すのは微妙かも。
`jsonequal.ShouldBeSame()` と `jsonequal.Equal()` に分けた。

:thought_balloon: integration testとunit testを書くのは良い感じかも。
(example testに変更していった方が良いような気もしている)

### transformer的な何か

これは何が作りたいんだろうな２つのことが混ざっている気がする。

- 比較対象を全体ではなく一部にしたい
- 比較対象での冪等性が担保されていない部分を差し替えたい

### refs

とりあえずreferenceを元に置き換えるようなものを作ろう。↑の後者の方。

たとえばnowとかbson.ObjectIdを置き換えたい。静的な構造のものならreflect使って走査する必要ないかも。

位置の指定はどういう表現を使おうかな。。jsonpointerを使うか。。

何かライブラリを使えるかな。何かでwrapする感じの実装になっていなくて単にreflectを使ってparseするだけのものなら使いたいかも。

悪くないかも。

- https://github.com/xeipuuv/gojsonpointer

```
go get -v github.com/xeipuuv/gojsonpointer
```

## jsonpointer

そう言えばarrayの部分の仕様忘れがち。

- https://tools.ietf.org/html/draft-ietf-appsawg-json-pointer-07


```
   o  If the currently referenced value is a JSON array, the reference
      token MUST contain either:

      *  characters that represent an unsigned base-10 integer value
         (possibly with leading zeros), making the new referenced value
         the array element with the zero-based index identified by the
         token, or

      *  exactly the single character "-", making the new referenced
         value the (non-existant) member after the last array element.
```

対象がarrayの時に

- `\d+` が指定されていたらその位置に挿入 (飛び飛びの場合にはどうするんだろう？)
- `-` が指定されていたら末尾に挿入

## go rfc3339

そういえば全然関係ないけれど、コードを書く時にめんどうなのでrfc3339用のtimeがほしいな。
MustParse()的なものが欲しいし、layoutはどうせrfc3339しか使わない。

はい。

https://github.com/podhmo/go-rfc3339

そういえば以下がすぐに手が動かない模様

- ci
- go modules 対応


