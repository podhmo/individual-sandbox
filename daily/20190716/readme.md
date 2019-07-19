## go interface, reflect

- sliceなどをloopする時にswitchで変換できないよね？
- mapくらいなら行ける？

encoding/jsonを見れば良い？

```go
```

あとはDeepEqualとか(reflect)

- https://golang.org/src/reflect/deepequal.go

- CanAddrのときには循環参照を気にして調べる
- Pointerのとき、Elem()でとりだす
- Interfaceのとき、IsNil()のあとにElem()
- Structのとき、NumField()とField()
- Sliceのとき、 Len()とIndex()、 事前に、IsNil()、Len()、Pointer()
- Arrayのとき、Len()とIndex()
- Mapのとき、MapKeys(),MapIndex()、 事前に、IsNil()、Len()、Pointer()
- Funcのとき、IsNilを見るくらいしかできない

