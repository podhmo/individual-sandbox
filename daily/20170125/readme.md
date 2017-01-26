# go JSONへのserializeの話

- 値の一部を消したりなど変更したい
- structであればembeddedなものを使ってどうにかできる
- *interfaceとstruct*でembeddedにした時のserialize結果が異なる

詳しくは[ここ](https://github.com/golang/go/issues/7230)
