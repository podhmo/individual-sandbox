# go では透過的なパッケージが作れない

python ではfoo moduleの全てを提供するbar moduleみたいなことは気軽にできる。

```python
# productionなどではやってはダメ
from foo import *  # NOQA
```

と言うより、importしてexportすればそれそのものの値をexportできる。

```python
# bar module
from foo import X  # NOQA
```

このユーザーは、barで定義してあるものであるかのように、以下の様に使える。

```python
import bar
bar.X

from bar import X
```

## そもそもgoの場合

type aliasは違う型。

```
type MyInt int
```

structに対してtype aliasしてもzero members。

```
type S2 S1
// S1のmethodは使えなくなる。
```

importに関しても同様で。現在のpackageにimportされるもののそれがそれとしてexportされることは無い。
(export対象にならない)

```
import . "foo"
```


# go structを返すような関数はerrorを返す時にはzero valueを返す。

今までpointerを返すものばかり使っていたので気づかなかった。少しキモい。
けれど、確かにerrがnil以外かをチェックするだけなので失敗時の値はなんでも良いといえばなんでも良い。

```go
type Ob struct {
}

func g() (Ob, error) {
	// error
	// return nil, nil
	return Ob{}, nil
}

// (*Ob, error) を返す関数ならnilでいける
```

以下ではnilがOK。

- pointer
- interface

よく考えたら当たり前で。値が無い状態ということを値を直接持つものでは作れない。
これは例えばprimitiveな型(e.g. int)でも同様。


# mongodb かえってきたデータ整形する

```
> db.<collection>.find().pretty()
```

# python yaml.loadがsyntax error返さない？

例: Noneになる

```python
import yaml
from io import StringIO

s = """
foo:

bar:
 x: y
"""

data = yaml.load(StringIO(s))
print(data)  # {'foo': None, 'bar': {'x': 'y'}}
```

