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
