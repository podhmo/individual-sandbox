## 作業

- metashapeのmypyを付ける
- mypyを手軽にエディタから使いたい
- flycheck-compileがvenvを気にしない
- metashapeのことを考える
- poetryのnewしたときの挙動を確認する

## poetry

- https://github.com/sdispater/poetry

```
[tool.poetry.scripts]
poetry = "poetry.console:main"
```

内部的には[cleo](https://github.com/sdispater/cleo)を使っているのか

全部のcommandsがcommands以下にある

https://github.com/sdispater/poetry/blob/master/poetry/console/application.py#L16

内部の構造をどうしているかと言うとLayoutというclassに保持している。

こいつは内部のものは全部tomlで書かれている。単純。なるほど。[tomlkit](https://github.com/sdispater/tomlkit)というものが使われている。


## emacs mypy

- flycheck-python-mypyがある

venvを気にして欲しい

こいつが呼ばれている。

- flycheck-checker-shell-command

一応hookは存在する？

```
                   (funcall flycheck-command-wrapper-function
                            (cons (flycheck-checker-executable checker) args))
```

## emacs 良い感じのlint環境を

- flycheck/flymakeでのlinterをminor modeによって選択したい
- emacsを使っている理由の一つとして気に入らない体験をomitできる（はず）ということがあげられる（？）
- プロジェクト毎にcheckerに渡すオプションが固定なのがつらい
- mypy.iniを探して欲しい。何なら関数を渡したい？

## emacs 良い感じのLS環境を

- 一時的にtimeoutを無効にしたい
- 現状の進捗を表示して欲しい
- 省コストモードを実装して欲しい。特定のバッファだけを気にするようなもの。

