# python3.13からlib2to3がなくなった

- 最もミニマムなhatchを使ったpyproject.tomlを作ってみる。
- lib2to3のimportが可能かを調べる。

```sh
$ uvx hatch shell py3.13
$ hatch project metadata
$ hatch test --show
$ hatch test
```

# 3.13ではlib2to3が存在しない

- [What’s New In Python 3.13 — Python 3.13.5 documentation](https://docs.python.org/3/whatsnew/3.13.html#to3)

```sh
$ PYTHONDEVMODE=1 hatch run python -c 'import lib2to3 as m; print(m.__name__)' 
───────────────────────────────────────────────────────── py3.12 ─────────────────────────────────────────────────────────
<string>:1: DeprecationWarning: lib2to3 package is deprecated and may not be able to parse Python 3.10+
lib2to3
───────────────────────────────────────────────────────── py3.13 ─────────────────────────────────────────────────────────
Traceback (most recent call last):
  File "<string>", line 1, in <module>
    import lib2to3 as m; print(m.__name__)
    ^^^^^^^^^^^^^^^^^^^
ModuleNotFoundError: No module named 'lib2to3'
```

blackをインストールしてblib2to3を使えばどうにかなりそう。


# report.md

これは動作確認のために出力したもの。本来は不要。

