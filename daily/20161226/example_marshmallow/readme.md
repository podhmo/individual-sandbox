- load時のdefault -> default
- dump時のdefault -> missing

default

```bash
$ python 00dump_default.py
input {}
loaded {}
dumped {'name': 'foo'}
```

missing

```bash
$ python 01load_default.py
input {}
loaded {'name': 'foo'}
dumped {'name': 'foo'}
```
