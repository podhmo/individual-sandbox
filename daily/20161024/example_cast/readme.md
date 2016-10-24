型の変換のやり方

0. source部分とdestination部分のpointerのレベル値を計算する
0. source部分の型をcodeに置き換える
0. 型変換*の部分をcodeに置き換える
0. destination部分の型をcodeに置き換える
0. [`2.`, `3.`, `4.`] の形で結合


例 `// *int => Z` ここで　`int -> Y; Y -> Z`

## 1.

```
src : int [0]
dst : Z [0]
```

pointerのレベルは同じなので気にしなくて良い(00main.goのPXみたいなやつは気にしないとダメ)

## 2.

```
*int : [(deref)]
```

## 3.

```
int -> Y -> Z : [(coerce int Y), (coerce Y Z)]
```
推移性はあるようなので以下でOK

```
[(coerce int Z)]
```

これは `[(cast Z)]`

## 4.

```
Z : []
```

## 5.

```
[(2.), (3.), (4.)] = [(deref), (cast Z)]
```

なので `*int -> Z` は以下の様になる。

```
type Y int
type Z Y
_x := 1
x := &_x // *int
fmt.Printf("%T => %T\n", x, Z(*x)) // *int => main.Z
```
