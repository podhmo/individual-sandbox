## dictknife stream

- そろそろ名前を決めたいところだけれど
- とりあえずemitしたコードを眺めて気になるところを直していく

  - runtime.resolve_xxx()という形に揃えるか
  
- 生成されたコードがデータ無しにiterateも可能であってほしいのだよなー。

  - xxx_of_のbodyを表示するようにしてみた
  
- linkを直す
  - Noneのときのも繋げるようにすれば良い。

- bug
  - prestring.Moduleの管理をclsnameでやっていた
  - delayedの部分ちょっと無駄
  - registered visitorとregisteredおんなじものでは？

- nestしたときのarrayがおかしい
- たぶん存在していないrefが現れることがあるoneOfの時に暗黙の定義をしていた場合
- additionalPropertiesの候補はpropertiesとpatternPropertiesに含まれなかったもの

## validation

- schema
- composite schema
- predicate, position
- predicateにpositionを与えるとfieldになる
- predicateはboolではなくerrorのリストを返す(predicateとは？)
