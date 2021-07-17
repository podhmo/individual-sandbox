## python windows インタラクティブシェルで行頭移動などのキーが効かない

bash gitで立ち上げたbashでpythonのインタラクティブシェルを立ち上げた場合。

### 追記

msvcrtというものを使っている？

- [How do I detect Ctrl+Key in Python without having to download other modules? - Stack Overflow](https://stackoverflow.com/questions/61471421/how-do-i-detect-ctrlkey-in-python-without-having-to-download-other-modules)

この方法は動くらしい。

### 追記

https://docs.python.org/ja/3/library/readline.html

```py
import atexit
import os
import readline

histfile = os.path.join(os.path.expanduser("~"), ".python_history")
try:
    readline.read_history_file(histfile)
    # default history len is -1 (infinite), which may grow unruly
    readline.set_history_length(1000)
except FileNotFoundError:
    pass

atexit.register(readline.write_history_file, histfile)
```

readlineはそもそもダメっぽいな。インストールもされてなかった。

```console
ModuleNotFoundError: No module named 'readline'
```

### 追記

https://stackoverflow.com/questions/51157443/pythons-readline-module-not-available-for-windows

pyreadline?

## python windows 何かdefaultのencodingがUTF-8じゃないっぽい？

```console
$ ~/.local/pipx/venvs/shosai/Scripts/shosai hatena push --publish about-terraform-nested-output.md
extra commands module is not found (shosai.hatena.extra_commands)
INFO:shosai.hatena.configuration:read: C:\Users\<name>/.config/shosai/config.json
Traceback (most recent call last):
  File "C:\Users\<name>\.local\pipx\venvs\shosai\Scripts\shosai-script.py", line 33, in <module>
    sys.exit(load_entry_point('shosai', 'console_scripts', 'shosai')())
  File "c:\users\<name>\ghq\github.com\podhmo\shosai\shosai\commands\shosai.py", line 215, in main
    return submain(args.service, rest_argv)
  File "c:\users\<name>\ghq\github.com\podhmo\shosai\shosai\commands\shosai.py", line 300, in submain
    return params.pop("subcommand")(service, **params)
  File "c:\users\<name>\ghq\github.com\podhmo\shosai\shosai\commands\shosai.py", line 137, in push
    parsed = parsing.parse_article(rf.read())
UnicodeDecodeError: 'cp932' codec can't decode byte 0x81 in position 59: illegal multibyte sequence
```

うーん？

```console
>>> sys.getdefaultencoding()
'utf-8'

>>> import pathlib
>>> open(pathlib.Path("~/.config/shosai/config.json").expanduser())
<_io.TextIOWrapper name='C:\\Users\\nao\\.config\\shosai\\config.json' mode='r' encoding='cp932'>
```

https://docs.python.org/ja/3/library/functions.html?highlight=open#open

> encoding が指定されていない場合に使われるエンコーディングはプラットフォームに依存します:locale.getpreferredencoding(False) を使って現在のロケールエンコーディングを取得します。


```console
>>> import locale
>>> locale.getpreferredencoding(False)
'cp932'
```

- https://docs.python.org/ja/3/library/locale.html#locale.getpreferredencoding

`PYTHONUTF8=1` か `-X utf8` を指定すれば良いのか。

- [Windows 上の Python で UTF-8 をデフォルトにする - Qiita](https://qiita.com/methane/items/9a19ddf615089b071e71)
- https://docs.python.org/ja/3/using/cmdline.html#envvar-PYTHONUTF8

```console
$ python -X utf8
Python 3.9.6 (tags/v3.9.6:db3ff76, Jun 28 2021, 15:26:21) [MSC v.1929 64 bit (AMD64)] on win32
Type "help", "copyright", "credits" or "license" for more information.
>>> import pathlib
>>> open(pathlib.Path("~/.config/shosai/config.json").expanduser())
<_io.TextIOWrapper name='C:\\Users\\nao\\.config\\shosai\\config.json' mode='r' encoding='UTF-8'>
>>> exit()
```

良さそう。

### 環境変数の設定

powershellで

```
[Environment]::SetEnvironmentVariable("PYTHONUTF8", ”1”, 'User')
$ENV:PYTHONUTF8
```

git bashはこれでは設定されない？

```console
$ python -c 'from pathlib import Path; print(Path("~/.config/shosai/config.json").expanduser().open())'
```