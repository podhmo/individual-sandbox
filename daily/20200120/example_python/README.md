## namespace packageについて言及して欲しい

`__init__.py`の省略は暗黙にnamespace packageとして扱われる機能であって別物。
python3.xから不要になるという説明は単純に誤り。

```
$ tree -I "__pycache__" -P "*.py" .
.
├── 01use.py
├── foo-bar
│   ├── foo
│   │   └── bar.py
│   └── setup.py
└── foo-boo
    ├── foo
    │   └── boo.py
    └── setup.py
```

01use.py。namespace packageは複数のpackageに跨るpackageなので `__file__` を持たない。

```console
$ python 01use.py
bar VENV/individual-sandbox/daily/20200120/example_python/foo-bar/foo/bar.py
boo VENV/individual-sandbox/daily/20200120/example_python/foo-boo/foo/boo.py
None
```
