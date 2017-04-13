# python dict上の言語について考えていた

- どこまでやるのかむずかしい
- マクロっぽい機能導入してみる？(言語を拡張する機能を言語内に組み込む)
- シンプルなところから考える。
- importできるようにしたい
- 引数取れるようにしたい
- 変数や条件分岐やループは？？
- やっぱりtemplateっぽいこともしたい？

## シンプルなところから考える。

### dict

dictに対する演算を定義したい場合には演算子の名前が消えてしまって良い。

```yaml
$addX:
  person:
    name: foo
```

を

```yaml
personX:
  name: foo
```

に。この場合はupdateになる。

### list

listに対する演算も定義したい

```yaml
items:
  $twice:
    - x
    - y
```

を

```yaml
items:
  - x
  - y
```

に。

この時はdictがlistに代わる。updateではおかしくなる。
また、戻り値がlistのものと一緒にdictの定義もあるとおかしくなる？


```yaml
items:
  $twice:
    - x
    - y
  foo: bar
```

これはキーワード引数として扱えば良いのか。

```yaml
$foo:
  x: 10
x: y
```

この場合は、xはキーワード引数として扱われてほしい。

## 結局

真面目に書くと

```yaml
$call:
  name: suffix
  kwargs:
    suffix: +
  body:
    person:
      name: foo
```

が以下の様な呼び出しになり

```python
suffix(d, sep="+")
```

こういう出力。

```yaml
person+
  name: foo
```

になるのが正しい？でも$callがsquashされちゃうので結局同じ階層のものは何も使えない。なので元の通りこれで良い感じ。

```yaml
$suffix:
  person:
    name: foo
suffix: +
```

複数の操作を同時にやるのは？

```yaml
$suffix:
  $suffix:
    person:
      name: foo
  suffix: +
suffix: +
```

微妙に分かりづらい。引数と関数が離れていくのが辛い。そして実行の順序が内から外なのが辛い。
clojureのthreadingマクロのようなもの。

```yaml
$chain:
  person:
    name: foo
    age: 20
body:
  - $suffix:
    suffix: +
  - $suffix:
    suffix: +
```
evalator自身を取れないと。こういうマクロ定義できない。

こうもできたほうがよいかもしれない。

```yaml
$chain:
  person:
    name: foo
    age: 20
body:
  - $suffix: {suffix: +}
  - $suffix: {suffix: +}
```
