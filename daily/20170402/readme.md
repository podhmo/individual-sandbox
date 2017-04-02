## JSON json merge と json patch

- json merge https://tools.ietf.org/html/rfc7396
- json patch https://tools.ietf.org/html/rfc6902

## $refのはなし

- https://github.com/json-schema-org/json-schema-spec/issues/66

## python indent

ちょっとindentしたくなった場合に便利

```python
import sys
import contextlib
from io import StringIO


@contextlib.contextmanager
def indent(n):
    buf = StringIO()
    with contextlib.redirect_stdout(buf):
        yield buf
    buf.seek(0)

    prefix = " " * n
    write = sys.stdout.write
    for line in buf:
        write(prefix)
        write(line)
    sys.stdout.flush()
```

```python

print("a")
with indent(2):
    print("b")
    with indent(2):
        print("c")
    print("d")
print("e")

"""
a
  b
    c
  d
e
"""
```

## python tempdir

複数のファイル弄るときとかに便利

```python
from tempfile import TemporaryDirectory


with TemporaryDirectory() as d:
    do_something()
```

## python pathlib

慣れるとos.path使ったり云々みたいなものが無くなりそう？

```python
from pathlib import Path

d = Path(d)
i = 3

with d.joinpath("{:02}.txt".format(i)).open("w") as wf:
    wf.write("i: {}".format(i))


with d.joinpath("05.txt").open() as rf:
    print(rf.read())
```
