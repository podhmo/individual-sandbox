## make shell timeの結果をファイルに追記したい

```make
default:
	(time echo foo) &>> /tmp/xxx
	(time echo foo) >> /tmp/xxx 2>&1  # これと同じ
```

## shell emacs 上で立ち上げた "*shell*" のshellのlocaleを変えたい

```
LC_ALL=
```

localeの結果

```
$ locale
LANG=en_US.UTF-8
LC_CTYPE=ja_JP.UTF-8
LC_NUMERIC=ja_JP.UTF-8
LC_TIME=ja_JP.UTF-8
LC_COLLATE="en_US.UTF-8"
LC_MONETARY=ja_JP.UTF-8
LC_MESSAGES="en_US.UTF-8"
LC_PAPER=ja_JP.UTF-8
LC_NAME="en_US.UTF-8"
LC_ADDRESS="en_US.UTF-8"
LC_TELEPHONE="en_US.UTF-8"
LC_MEASUREMENT=ja_JP.UTF-8
LC_IDENTIFICATION="en_US.UTF-8"
LC_ALL=
```

## arch pacmanでどのパッケージでインストールされたコマンドか調べる

```console
$ sudo pacman -Fy
:: Synchronizing package databases...
 core                            806.8 KiB  4.72M/s 00:00 [##############################] 100%
 extra                             7.8 MiB  6.16M/s 00:01 [##############################] 100%
 community                        18.3 MiB  9.94M/s 00:02 [##############################] 100%
$ sudo pacman -Fo `which ls`
usr/bin/ls is owned by core/coreutils 8.30-1
```

## python socket.socket.sendfile

http://michaldul.com/python/sendfile/

2倍位早くなるらしい

### socket.listen()の引数ってなんだっけ？

backlog。acceptが呼ばれるまでのTCP connectionの待ち行列のキューの長さ。

## python jsonrpcの使いかた

```
pip install python-jsonrpc-server
```

pylsでどう使われているか見る。(MethodDispatcher)

どちらかと言うと、以下が便利？

```
from jsonrpc.endpoint import Endpoint
from jsonrpc.streams import JsonRpcStreamReader, JsonRpcStreamWriter
```

jsonrpcの部分を把握しないとテストが書けないじゃん。

## python pyls

もしかしてjediのcompletion壊れている？
デフォルトではropeを使っているっぽい？いや、install_requiresにjediが存在している。ropeはextras_require

## python pluggy hookの登録など関数ベースでできる？

オブジェクトで定義するのが面倒。１つの方法としてmodule利用するという方法はありそう。

## python pluggyの使いかた

pluggyはpytestのplugin機構をになっているライブラリ。

https://pluggy.readthedocs.io/en/latest/

その前に名前の確認

- host
- plugin

提供しているのは

- PluginManager
- Spec
- Plugin

specはpluginが満たしたい仕様的なもの。
specにはspec markerをpluginにはhook implをそれぞれデコレーターを指定するっぽい。

実際に繋げるのはmanagerへのregisterが必要。hookを探すのは`manager.hook.<hook name>`。１つを探すのではなく、全部を実行する感じっぽい(fan-out的な)。

必ずmethod名

examplesは https://github.com/pytest-dev/pluggy/tree/master/docs/examples あたりにある。



## python python-language-server

```console
$ pip install python-language-server
$ which pyls

# defaultはstdin/stdout
$ pyls
# tcpを指定するとネットワーク越しに使える(socketserver.TCPServer)
$ pyls --tcp --host 127.0.0.1
```

### 内部のコード

completionの部分を見る

- python_ls.py
- plugins/rope_completion.py
- plugins/jedi_completion.py

capabilityの指定が以下。resolveProviderってなんだろう？

```python
  'completionProvider': {
      'resolveProvider': False,  # We know everything ahead of time
      'triggerCharacters': ['.']
  },
```

pluginsとpython_lsとの組み合わせどうなっているんだろう？

```python
class PythonLanguageServer(MethodDispatcher):
    def _hook(self, hook_name, doc_uri=None, **kwargs):
        """Calls hook_name and returns a list of results from all registered handlers"""
        doc = self.workspace.get_document(doc_uri) if doc_uri else None
        hook_handlers = self.config.plugin_manager.subset_hook_caller(hook_name, self.config.disabled_plugins)
        return hook_handlers(config=self.config, workspace=self.workspace, document=doc, **kwargs)

    def completions(self, doc_uri, position):
        completions = self._hook('pyls_completions', doc_uri, position=position)
        return {
            'isIncomplete': False,
            'items': flatten(completions)
        }
```

内部的には`"pyls_completions"`のhookで呼ばれる。

plugin managerってどうしているんだろう？(`subset_hook_caller()`)

```
import pluggy
from pyls import PYLS
# PYLSは"pyls"
pm = pluggy.PluginManager(PYLS)
```

あと、setup.pyでentry_pointsの指定が存在する。

```python
  'pyls': [
...
      'jedi_completion = pyls.plugins.jedi_completion',
...
      'rope_completion = pyls.plugins.rope_completion',
...
]
```

hookの登録について見てみるとhookimplのデコレータ付ける形の模様。ここでhookimplなどの定義は以下。

```python
hookspec = pluggy.HookspecMarker(PYLS)
hookimpl = pluggy.HookimplMarker(PYLS)
```

pyls/plugins/jedi_completion.py

```python
from pyls import hookimpl

@hookimpl
def pyls_completions(document, position):
    definitions = document.jedi_script(position).completions()
    return [{
        'label': _label(d),
        'kind': _kind(d),
        'detail': _detail(d),
        'documentation': _utils.format_docstring(d.docstring()),
        'sortText': _sort_text(d),
        'insertText': d.name
    } for d in definitions] or None
```

### eglotのテストを手動でやってみる

[../20190104/readme.md](../20190104/readme.md)

```python
import sys
sys.exi
```

:completionProvider
