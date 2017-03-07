# python mock unittest.mockで特定のインスタンスを継承した仕組みのものを作りたい

このようなクラスがある時に絶対に `self.g()` を呼び出したくない。一方でfはそのまま呼び出したい。

```python
class A(object):
    def f(self):
        print("f", self)
        return self.g()

    def g(self):
        print("g", self)
        return ["g"]
```

普通にAを継承したMockAを作った場合には、fの内部で呼ばれるメソッドがgではなくなった場合に困る。

```python
class A(object):
    def f(self):
        print("f", self)
        return self.h()  # hに代わる

    def h(self):
        print("h", self)
        return ["h"]

class MockA(A):
    def g(self):  # これが呼ばれてほしいが。fから呼ばれるのはA.h。
        print("g", self)
        return ["g"]

```

苦肉の策で以下のような方法はある(他にもpatchでnew_callableとかnewを指定するのでいける気がするけれど。大げさ)。

```python
from functools import partial
import unittest.mock as mock


def create_mock():
    m = mock.Mock(spec_set=A)
    m.f = partial(A.f, m)
    return m
```

色々汎用化すると

```python
def handmade_inheritance_mock(cls, attributes):
    m = mock.Mock(spec_set=cls)
    m.configure_mock(**{k: partial(getattr(cls, k), m) for k in attributes})
    return m
```
