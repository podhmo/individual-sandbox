## egoist validation

- https://github.com/podhmo/maperr

これと組み合わせるか。
structの定義はできていて、定数部分の表示はまだという感じ。

あと、存在を忘れそうなので記事を書いておくか。

### 追記 UnmarshalJSON()の作成

どのようなコードが生成される予定なのかはコメントで書いてしまったほうがわかりやすいかもしれない。
あと、metadataのdefaultをwalkで注入したほうがきれいなのではないか？
（存在しない場合にはrequiredなどは分かりづらい）
