## go types

普通にtypesを使う方針で何故ダメだったのか？

- typesの無い状態でも利用したい（？）
- 何をどこまで利用する？

e.g.

- typeの定義はtypes.Type?
- symbolの定義は自分でやる？ or types.Var?
- どういう条件があった？

```
package main

import (
	"m/foo"
	bar "m/bar/foo"
)

func main(x foo.X, y bar.Y){
  bar.Z0 // この型は foo.Z
}

```


どこにパッケージがあるか？

- typeの定義先
- symbolの定義先 (typeの利用先)

imported symbol, imported packageはなぜ必要？
prefixの判定にはどこへimportされたかの情報が必要だから。

### 追記

普通の利用のアナロジーで考えることはできる？これではだめなんだろうか？
importしたものが使えるものを指定できない。文字列での指定になる。

```go
foo := File.Import(fooPkg)
foo.XXX
```

