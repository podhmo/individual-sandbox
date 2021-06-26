#[pip][python]pipxのススメ

[https://pypa.github.io/pipx:cite:embed]

pipxという名前を聞いてまたpythonの新しいパッケージ管理のツールと思うかもしれないけれど。少しだけ用途が違うのでこれがほしい場合がある。

pipxは例えばpoetryやpipenvとはちょっと用途が違う。

pipxは日常生活のためのCLIコマンドのためのもので、pipenvやpoetryはアプリケーション開発のためのものといえば良いのだろうか？

例えば、python製の環境を汚さず[^2]コマンドを利用したいときにpipxは便利。httpieだとか。sphinxだとか。blackだとかflake8だとか[^1]。
裏側では各コマンドごとのvenvを作ってくれてそのvenv内のscriptのsymlinkをいい感じに作成してくれる。

## 環境を汚さずコマンドをインストールしたいとき

defaultでは `$HOME/.local/bin` 以下にコマンドが置かれる。

```console
$ python -m pipx install jqfpy
$ which jqfpy
$HOME/.local/bin/jqfpy
```

どのような環境が作られているかは list で見れる。

```console
$ python -m pipx list
venvs are in C:\Users\podhmo\.local\pipx\venvs
apps are exposed on your $PATH at C:\Users\podhmo\scoop\apps\python\3.9.5
   package flake8 3.9.2, Python 3.9.5
    - flake8.exe
   package jqfpy 0.6.2, Python 3.9.5
    - jqfpy.exe
```

## あるコマンドの環境に依存するパッケージを追加したいとき

例えば、sphinxなどでpluginを使いたくなることがある。その環境下に余分に依存を追加したい。こういうときにはinjectを使う。

```console
$ python -m pipx inject sphinx sphinx-<plugin>
```

## windowsの場合

ここからはかなり個人的な環境の話になるが、windowsの場合にもちょっとだけ気をつけると便利に使えるようになる。

最近、というか今日家のmacが壊れたのでwindowsのPCを使い始めたのだけれど、パッケージの管理にscoopを使っている。

[https://scoop.sh/:cite:embed]

これはこれで便利なのだけれど。これでインストールしたpythonといい感じに共有したい。ところで、このscoopはcurrentの部分にPATHが通るように設定してくれる。

```console
$ scoop install python
$ which python
C:/Users/podhmo/scoop/apps/python/current/python.exe
```

ここでpipxをインストールしたとき `--user` をつけてインストールしたときにはcurrentではない場所にpipxが置かれる。もちろん、PATHを設定すれば良いことなのだけれど。めんどくさいので省略したい。幸いpipxは `python -m pipx` でも動くのでこちらを使う。

```console
$ python -m pip install --user pipx
...
  WARNING: The script pipx.exe is installed in 'C:\Users\podhmo\AppData\Roaming\Python\Python39\Scripts' which is not on PATH.
  Consider adding this directory to PATH or, if you prefer to suppress this warning, use --no-warn-script-location.

# 無い
$ which pipx
# pythonからは呼べる
$ python -m pipx --version
0.16.3
```

同様に素直に `pipx install` でインストールすると `$HOME/.local/bin` に置かれてしまう。これには `PIPX_BIN_DIR` を指定してあげると良い（venvの位置も `PIPX_HOME` で変えれるが今回は気にしない）。

https://pypa.github.io/pipx/installation/#installation-options

> The default binary location for pipx-installed apps is ~/.local/bin. This can be overridden with the environment variable PIPX_BIN_DIR.

PowerShellには不慣れだが以下の様な感じで設定できるようだ。

```console
 $ $pydir = python -c 'import sys; import pathlib; print(pathlib.Path(sys.executable).parent)'
 $ $ENV:PIPX_BIN_DIR^$pydir
 $ python -m pipx instal jqfpy
```

scoopで通っているpathに置かれる様になったので直接呼べる。

```console
$ which jqfpy
C:/Users/podhmo/scoop/apps/python/current/jqfpy.exe
$ echo '{"foo": 1}' | jqfpy
{
    "foo": 1
}
```

[^1]: 真面目なアプリケーション開発でpythonのバージョンと各種devツールのバージョンを揃えたい場合には、それぞれのプロジェクトごとのvenvにインストールするべきかもしれないけれど。
[^2]: 環境を汚さないには２つの意味がある。一つはいわゆるpython環境を汚さないということ。もう一つはpyhtonはモジュールの持ち方がフラットなのでnodejsのように全体で同じパッケージの異なるバージョンに依存することができない。ここで環境を共有して同じ依存を含んだコマンドを２つインストールしてしまった場合に両者の依存が干渉してしまうことがある。これを避けるということ。