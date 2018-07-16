## python module

modulefinder というパッケージ知らなかった。

- [31.3. modulefinder --- スクリプト中で使われているモジュールを検索する — Python 3.6.6 ドキュメント](https://docs.python.org/3/library/modulefinder.html "31.3. modulefinder --- スクリプト中で使われているモジュールを検索する — Python 3.6.6 ドキュメント")

コマンドラインからでも使える。

```console
python -m modulefinder <file>
python -m modulefinder $(which jupyter-console)
```

## python jedi変更する

https://github.com/podhmo/emacs-jedi/tree/fix-venv

変更できたのだけれど。そもそも不要そうなオプションとか消したい気もしたり。

(find-file "./example_jedi/jedi-setup.el")
