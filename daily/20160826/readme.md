# python あえてnamedtuple(namedlist)を使う理由

## とりあえずの場合

特に何も考えずに値のコンテナが欲しい場合には、何も考えずにnamedtupleを使う場合がある。
以降はこれを抜きにした話。

## namedtuple

namedtupleを使いたい場合は、たいていの場合immutableなコードを意識したい時。
pythonのtupleは値の変更を許していない。この機能を継承している分値の変更を許したくない場合に使う。

## それでもobjectを定義せずにnamedtupleないしはnamedliistを使う場合

上の前提に立つなら、mutableなコードにしたい場合などには自分でクラスを定義すれば良いということになる。
しかし、そうではなく、namedtupleないしはnamedlistを使いたくなることがある。

それは比較の戦略を変えたい場合。通常のpythonのオブジェクトは参照の比較になってしまう。
そうではなく値オブジェクトのように値での比較を要求したい場合に便利。
tupleやlistの比較処理の実装は始端から終端まで順次調べる実装になっているので。

```python
A = namedtuple("A", "x y")
A(x=10, y=20) == A(x=10, y=20)  # => True
id(A(x=10, y=20)) == id(A(x=10, y=20))  # => False
```

## 注意

namedtupleに関しては本当にただのtupleなので以下がtrueになってしまうことに注意。

```python
from collections import namedtuple

A = namedtuple("A", "x y")
B = namedtuple("B", "x y")
C = namedtuple("C", "i j")

print(A(x=10, y=20) == A(x=10, y=20))  # => True
print(A(x=10, y=20) == B(x=10, y=20))  # => True
print(A(x=10, y=20) == C(i=10, j=20))  # => True
```


## 補足

namedlistは外部ライブラリ
