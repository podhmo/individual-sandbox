## go tinyorm -> miniq

こんな感じでまとめられる？

```
Do (ob interface{}, stmt string, args ...interface{}) error
DoWithValue (ob interface{}, stmt string, args ...interface{}) (interface{}, error)
DoWithValues (ob interface{}, stmt string, args ...interface{}) ([]interface{}, error)
```

mgoの方はどうだろう？

そして以下も必要だった

- OrderBy
- GroupBy
- Having

あとはjoin、prefixがつくのと、onでくっつける

- innerJoin
- leftOuterJoin
- rightOuterJoin
- fullOuterJoin

## go sql log

これめちゃくちゃ便利では？

- https://github.com/simukti/sqldb-logger

## go sql

- sqlboiler?
- gorp?
- sqlx

丁度良い。exampleがないな～。

- https://eli.thegreenplace.net/2019/to-orm-or-not-to-orm/
- insert時にそのIDが欲しかったりする

### 追記

気にしたいことはなんだろう？

- 自分でtableを作る必要があるかどうか
- insertした時点でのauto inserted id
- join
- query trace https://github.com/go-gorp/gorp#sql-logging
- hook https://github.com/go-gorp/gorp#hooks
- transactions https://github.com/go-gorp/gorp#hooks
- https://scrapbox.io/keroxp/Gorm%E3%81%8C%E6%9C%AC%E7%95%AA%E3%83%86%E3%83%BC%E3%83%96%E3%83%AB
%E3%81%AE%E6%95%B0%E5%84%84%E4%BB%B6%E3%81%AE%E3%83%87%E3%83%BC%E3%82%BF%E3%82%92%E6%B6%88%E3%81%9D%E3%81%86%E3%81%A8%E3%81%97%E3%81%9F%E8%A9%B1
- https://github.com/loilo-inc/exql

## goの生成 openAPI

とりあえず、生成を考えられるけれど。それ以外のことがしたい。
本当に大切なことは何なんだろうか？subprocessの展開？
separeted output?
