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

これはキーワード引数として扱えば良いのか。上の話は全部取りやめ。キーワード引数として扱うのが良さそう。

```yaml
$foo:
  x: 10
x: y
```

この場合は、外側のxはキーワード引数として扱われてほしい。

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
suffix(d, suffix="+")
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

## もう少しだけ欲張って

もう少し欲張った機能

- config読めるようにしたい
- というか、load用の機能として以下があるかもしれない

  - dataとしてload(変数束縛, configとしてload)
  - yamlとしてload(文字データ)
  - codeとしてload(import)

- moduleを読み込めるようにすると楽そう
- yaml上でjinja2

### moduleを読み込めるように

```bash
$ toylang foo.py data.yaml
```

foo.py

```python
def suffix(d, suffix="+"):
    return {k+suffix:v for k,v in d.items()}
```

chainとかするためにevalatorが欲しい？

```python
@with_evalator
def chain(d, evalator, body=None):
    return evalator.eval()
```

あ。chainに渡す式は評価されるとまずそう。$$とかでquote出来るようにするか。

.も許可すると良いのでは？例えば以下の様に書いたものがある場合。

```python
import f
```

こういう感じで呼べる。

```yaml
$f.something:
  hai
```

importなどの対応をするときなどにもたぶん楽。


### yaml上でjinja2

こんな感じに書きたい

```yaml
$define-template:
  body: |
    items:
    {% for i in nums %}
      - {{prefix|default("no")}}.{{i}}
    {% endfor %}
  name: person.jinja2

listing:
  $template:
  template: person.jinja2
  nums: [1,2,3]
```

結果はこうなるイメージ。

```yaml
listing:
  items:
    - no.1
    - no.2
    - no.3
```

### 本当はこう書きたい

同じ引数２個書きたくない。本当はこう書きたい。第一引数は必ずdictみたいな条件を緩和しないとダメ？

```yaml
listing:
  $template: person.jinja2
  nums: [1,2,3]
```

xamlとおんなじ感じで。何か特権的なbodyを許可しても良いかも？

```yaml
$chain:
  person:
    name: foo
    age: 20
body:
  - $$suffix: {suffix: +}
  - $$suffix: {suffix: +}

# こう書ける？

$chain:
  person:
    name: foo
    age: 20
body:
  - $$suffix: +
  - $$suffix: +
```

これはどうやって書けば良いんだろうな？こう？

```python
@default_option("suffix")
def suffix(d, suffix="+"):
    ...
```
