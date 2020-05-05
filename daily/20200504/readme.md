## egoist validation

- https://github.com/podhmo/maperr

これと組み合わせるか。
structの定義はできていて、定数部分の表示はまだという感じ。

あと、存在を忘れそうなので記事を書いておくか。

### 追記 UnmarshalJSON()の作成

どのようなコードが生成される予定なのかはコメントで書いてしまったほうがわかりやすいかもしれない。
あと、metadataのdefaultをwalkで注入したほうがきれいなのではないか？
（存在しない場合にはrequiredなどは分かりづらい）

### 追記　required/unrequired

これどこかで保持しておきたいな。

- metadataを見る
- metadataなしの場合にはrequired = true
- t.Optionalの場合にはrequired = false

### 追記 nestした表現

json.RawMessageを経由したいのだった。

- https://pod.hatenablog.com/entry/2019/08/30/230551

あと、errmapってこの形に対応してたっけ。

対応しないとまずいやつ

- object,pointer
- slices,pointer,primitive
- map,pointer,primitive
- (nested)

## 追記

けっこう頑張ったけれど。enumの対応がまだ

- *stringで受け取ってしまう
- oneOfのvalidationがつかない

