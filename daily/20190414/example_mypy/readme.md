## mypyを部分的に使う方法を調べる

具体的には通信部分を全部protocolベースにして境界を設けて境界内だけはまじめに型チェックを導入したい。

## それ以前の話

version

```console
$ pip freeze | grep -Pi '(mypy|mypy-extensions|typing-extensions)'
mypy==0.700
mypy-extensions==0.4.1
typing-extensions==3.7.2
```

## なぜかeditable installしたpackageはcannnot find module

```console
$ mypy 00fetch.py
00fetch.py:1: error: Cannot find module named 'foo'
00fetch.py:1: note: See https://mypy.readthedocs.io/en/latest/running_mypy.html#missing-imports
```

一応こうやれば動くけれど。他には思いつかないかも。

```console
$ MYPYPATH=foo mypy 00fetch.py
```

## xxx

- https://mypy.readthedocs.io/en/latest/running_mypy.html#mapping-paths-to-modules
