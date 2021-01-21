## aws cdk

rfc

- https://github.com/aws/aws-cdk-rfcs

## go params to path

良い名前が思いつかない

用途どんなものがあるだろう？

```go
type P struct {
    ID `json:"id" index:"literal"`
	Grade string `json:"grade" index:"enum" enum:"S,A,B,C,D,E"`
	Type string `json:"type" index:"dynamic-enum"`
}
```

- 指定できるindexのタイプは、literal,enum,dynamic-enum がある

### resolve

suffix is "data.json"

- `P{}` =>  `/id/ANY/grade/ANY/type/ANY/data.json`
- `P{ID: "a"}` => `/id/a/grade/ANY/type/ANY/data.json`
- `P{ID: "a", Grade: "C"}` => `/id/a/grade/C/type/ANY/data.json`

### iterate (breakdowns)

literalはパラメーターとして必須 (それともANYで扱う？)。
enum, dynamic-enumは、分解できる。
dynamicはどこかに保存される。(今回は X,Y,Z)
順序は必ず元のstructのフィールドの順序で固定される。 (`["grade","type"]`も`["type", "grade"]`も`["grade", "type"]`として扱う)

- `breakdown=["grade"]` -> `(S,), (A,), (B,), (C,), (D,), (E,)`
- `breakdown=["type"]` -> `(X,), (Y,), (Z,)`
- `breakdown=["grade", "type"]` -> `(S,X,), (A,X,), (B,X,), (C,X,), (D,X,), (E,X,), (S,Y,), (A,Y,), (B,Y,), (C,Y,), (D,Y,), (E,Y,), (S,Z,), (A,Z,), (B,Z,), (C,Z,), (D,Z,), (E,Z,)`

### config

- path化する時のseparator defaultは"/"
- zero値の位置の文字列表現 defaultは"ANY"
- tagから読み込む時のprefix defaultは""
- suffix

### どうする？

- pagination
- sort
- condition (e.g. < > >=, <=, and, or)
- field一個追加したら全部のパスが変わる (まじ？)

### どういうときに使える？

- 速度を気にしないKVS。Vのサイズは大きめ。
- Keyをもう少し柔軟にしたい。パラメーターで扱いたい。

## s3 kvs

- https://qiita.com/rmanzoku/items/b550a8dc79d67f3fa325
