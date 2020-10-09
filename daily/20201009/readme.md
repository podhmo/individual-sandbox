## go rcmp

とりあえず、DeepEqualのテストが通れば良いのでは？


## go reflect compare

真面目にreflect.DeepEqualのコードを読んでみるか。

いくつかわからないこと

- visitという構造体
- 謎のunexported function
- uintptr, unsafe.Pointer

呼び出し関係

1. `reflect.DeepEqual(x,y)`
2. `reflect.deepEqual(v1,v2,{},depth)`

### DeepEqual

- x or yのnil check
- `interface {}` -> `reflect.Value`
- 型が違ったらfalse

### deepEqual

```
func deepValueEqual(v1, v2 Value, visited map[visit]bool, depth int) bool
```

ほぼコレが本体

- depthはdebug用
- visitedのmapの意味がわからない
- hardという関数がわからない。

  - Map,Slice,Ptr,Interfaceでnilでなければtrue

- ptrvalという関数がわからない

  - reflect.Valueのunsafe.Pointerを取り出す
  - その後にunitptrでsortしてる。

- interfaceのときの比較が難しい。

内部の分岐

- v1 or v2 がinvalid?

  - どちらもinvalid?

- v1 or v2 のtype check
- hardかどうかを比較

  - hardのとき

    - ptrval()を使ってunsafe.Pointerを取り出す
    - unsafe.Pointerを昇順に揃える
    - v1.Type()で取り出した型とaddr1,addr2をvisitに入れる。これをkeyとして持っていたらtrue

  - Arrayのとき -> Len(), Index()
  - Sliceのとき -> nil check, len check, pointer check, Len(), Index()
  - Interfaceのとき -> nil check, Elem()
  - Ptrのとき -> Pointer(), Elem()
  - Structのとき -> NumField(), Field()
  - Mapのとき -> nil check, len check, pointer check, MapKeys(), MapIndex()
  - Funcのとき -> nil check, 他は何もできない
  - それいがい (normal equality check) -> valueInterface() の戻り値をチェック


### valueInterface

```
func valueInterface(v Value, safe bool) interface{}
```

- v.flag == 0 -> panic
- safe && flagRO -> panic (unexportedをInterfaceを介してアクセスできない)
- flagMethod -> makeMethodValueで値に変換
- kind

  - Interfaceのとき -> いろいろcast
  - それ以外 -> packEface(v)

packEfaceってなんなんだろう。
深く知る必要がないなら `reflect.Value.Interface()` のこと。

### packEface

```
func packEface(v Value) interface{}
```

- `interface {}` のzero valueを作る
- これのunsafe.Pointerを `*emptyInterface` 型の値として保持
- ifaceIndir()で分岐

  - ifaceIndir のとき
  - flagIndir のとき

なにやっているかほんとにわかんない。対になっているのは `unpackEface()` っぽい。型から考えるとreflectとinterfaceの変換っぽい。

