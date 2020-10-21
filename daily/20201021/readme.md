## go bubbletea

- https://github.com/charmbracelet/bubbletea

こいつのサンプルを動かしてみたい。

## emacs eglot

実はeglotも使いこなすと便利なのでは？

- eglot-code-actions?
- eglot-find-declaration
- eglot-find-implementation
- eglot-help-at-point

普通にflymakeで良いのでは説はある。
eglotのprojectがmergeされる基準を把握するのが良さそう？

### efm-langserver

テキトーに動かしてみる。
.config/efm-langserver/config.yamlになにか書く。
いろいろ調べたがなぜかeglotで動かない。
原因を調べてみたところsetenvでpathが追加されていなかった。

## ほしい

links的なcli?command。
your own custom repository的な。

雑に言うと、cookiecutterのrepositoryを覚えるのが面倒。
変にdbでも管理したくない。githubをURL代わりにできない？
可能なら誰かの設定をfollowできる。

- follow
- sync

## 環境整備

- editorでLSPが使えるように eglot -> lsp-mode
- 手軽にlinterを追加 efm-language-serverのようなノリ
- emacsのパッケージ管理を換えたい -> leaf?
- bashの補完用のなにかが欲しい -> bash?

## bashの補完用の何か

```consolenn
$ compgen -d . aws aws.s3 aws.ecs ...
```

- サブコマンドの補完
- サブサブコマンドの補完
- flagsの補完
- positional argumentsの補完
- global flagsの補完

どこまでやるか。

- static // 全部を一気に生成する awsだと200個とかある
- cached dynamic // ./.config/compgen/default/<command> を見に行くとかで
- dynamic // コマンド

既存の補完はdynamicになりがちなのでだるい。dynamicな補完はおもためのprocessでは機能しない。

### 追記

昔にも書いてたじゃん。

https://gist.github.com/podhmo/37fe265460e3b6ef9b97e9886de3f356

## より手軽にpypi

s3pypiのアプローチがやっぱり良い。
cloudfrontではないほうが手軽？cloudfrontのほうが手軽?

api gateway経由での認証認可は無駄？

いろいろ考えたけど。

- 汎用的に使えるcognite用のproxyを作っておく
- 雑にAPI gatewayでpublicに作成
- proxyを指定してpipを実行
- (binaryにproxyを埋め込む, private onlyの認証ってできるんだろうか？)

というのが手軽そう。

## pypi custom pypiserver

- https://github.com/novemberfiveco/s3pypi
- https://github.com/pypiserver/pypiserver
- https://github.com/stevearc/pypicloud
- https://devpi.net/docs/devpi/devpi/latest/+doc/index.html
- https://github.com/colinhoglund/piprepo

### pep

- https://www.python.org/dev/peps/pep-0541/
- https://www.python.org/dev/peps/pep-0503/

### pypiserver

- https://javawithravi.com/setting-up-a-custom-pypi-server/
- https://javawithravi.com/pip-custom-pypi-server/

### 実際に使う

- https://packaging.python.org/guides/using-testpypi/

```console
$ pip install --index-url https://test.pypi.org/simple/ --extra-index-url https://pypi.org/simple your-package
```

.pypirc
```
[testpypi]
username = <your TestPyPI username>
```
