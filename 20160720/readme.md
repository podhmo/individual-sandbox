## 普通とかやり過ぎとか

[この辺の話](http://qiita.com/methane/items/fa5f9c9c31f7afcf2211)

## namedtupleの罠

```python
from collections import namedtuple


A = namedtuple("A", "x y")
B = namedtuple("B", "i j")


print(A(x=10, y=20) == B(i=10, j=20))  # => True
```

こうすれば

```python
def equal(a, b):
    is_same_class = issubclass(a.__class__, b.__class__) or issubclass(b.__class__, a.__class__)
    return is_same_class and a == b
```
