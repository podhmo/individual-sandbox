# metashape

- https://github.com/podhmo/metashape

## 追記

- marker部分などモジュールの構成を変えたい？
- aggressiveはmarkerを付ける操作の方が良い？
- 全部をwalkするようなものはツライ？
- (repositoryなどは消してしまいたい)

```console
$ tree metashape -I tests -I __init__.py -I __pycache__
metashape
├── analyze
│   ├── core.py
│   ├── __init__.py
│   ├── repository.py
│   ├── resolver.py
│   └── typeinfo.py
├── cli.py
├── compile.py
├── declarative.py
├── flavors
│   ├── __init__.py
│   └── openapi
│       ├── __init__.py
│       └── resolve.py
├── __init__.py
├── langhelpers.py
├── __main__.py
├── marker.py
├── py.typed
├── runtime.py
├── shortcuts.py
└── types.py

3 directories, 19 files
```

## 追記

とりあえず既存のコードをtypeinfoを利用する形に変更する

- 辞書の形だと補完が効かないので不便
- enumを取り出すのにOptionalが邪魔

## 追記

- やっぱりwalkしている最中のcontextは欲しい？(path)
