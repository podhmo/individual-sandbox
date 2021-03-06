## python sorted dedup

```python
def sorted_dedup(xs):
    return sorted(set(xs), key=xs.index)
```

## python ordered counter

```python
class OrderedCounter(Counter, OrderedDict):
    'Counter that remembers the order elements are first encountered'

    def __repr__(self):
        return '%s(%r)' % (self.__class__.__name__, OrderedDict(self))

    def __reduce__(self):
        return self.__class__, (OrderedDict(self),)
```

or

```python
from collections import OrderedDict, defaultdict
import itertools

it = iter([2, 1, 3, 2])
it0, it1 = itertools.tee(it, 2)
d = OrderedDict.fromkeys(it0)
c = defaultdict(int)
for x in it1:
    c[x] += 1
d.update(c)
```


## python defaultdict a-la-carte

### counter

```python
from collections import defaultdict

c = defaultdict(int)
```

### idmap

```python
from collections import defaultdict

idmap = defaultdict(lambda: len(idmap))
xs = ["x", "y", "z", "x", "x", "a"]

for x in xs:
    print(x, idmap[x])
```

### tree dict

```python
from collections import defaultdict

def tree():
    return defaultdict(tree)
```
