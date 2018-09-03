## mypy PEP561読んだりした

typed packageはpy.typedというファイルを作るという感じなのか。
個人的にはstub packagesをすすめていきたい感じ。

ためしたexample作った

- https://github.com/ethanhs/pep-561
- https://github.com/ethanhs/stub-package
- https://github.com/numpy/numpy-stubs/blob/master/setup.py

## typed-packagesとstubs-packages

- typed-packagesは型定義が含まれたパッケージのこと
- stubs-packagesは対象とするパッケージに対するスタブファイル(型情報)を提供するパッケージのこと

## subs-packages

- .pyiしか使えない
- パッケージ名の"<package name>-stubs" という名前が重要
- setup.pyでpackage_dataの指定が必要



## mypy --ignore-missing-imports 理解しきれていない気がする

上手くimportできていない。これ使わないほうが良い？follow-importsを使ったほうが良い？
follow-importsは以下のどれかの設定。

- normal
- silent
- skip
- error

moduleの解決がわかっていないのかも？
そもそもpackage周りのことについて完全に理解できていない？

```
lib
  - python3.7
  - mypy
```

のような設定があり。

```
$ cat python3.7/site-packages/<module>.egg-link
<path>
.
```

だけが存在するような形。このときにmypyはmoduleを見つけられない見たい？

- [Support egg installs with PEP 561 searching · Issue #5007 · python/mypy](https://github.com/python/mypy/issues/5007 "Support egg installs with PEP 561 searching · Issue #5007 · python/mypy")
- [PEP 561 -- Distributing and Packaging Type Information | Python.org](https://www.python.org/dev/peps/pep-0561/ "PEP 561 -- Distributing and Packaging Type Information | Python.org")
