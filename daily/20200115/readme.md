## websocket CLI

まだwebsocketがうまく使えていない気がする。

- https://github.com/joewalnes/websocketd
- https://pypi.org/project/websockets/

### python subprocess

- subprocess - Popen
- multiprocessing

いい感じにstdin,stdoutを消費したい。communicateとかブロックしちゃわない？

## python import time

```
python -X importtime
```

もしくは

```
PYTHONPROFILEIMPORTTIME=1 python
```

- https://docs.python.org/ja/3/using/cmdline.html#envvar-PYTHONPROFILEIMPORTTIME

## python typed_astって何か違いあるのだっけ？

使用感的な話。

```console
$ pip install typed_ast
$ python -m pyinspect list typed_ast
typed_ast
typed_ast.ast27
typed_ast.ast3
typed_ast.conversions
```

ふつうにparse,dumpがあってwalkとNodeTransformerがある感じ。
astだなー。`Assign` と `AnAssign` があるのがtypedっぽい。

あと`get_docstring()`がある。

## python black

- run in executor + ProcessPoolExecutorで並行処理している
- https://github.com/psf/black/blob/master/black.py

### ライブラリとして使える？

ちょっと無理そう。black.pyに直書き的な感じ。

## python

これらを良い感じに取り扱いたいよね。

- blackd
- mypyd

## python grpc

- https://grpc.io/docs/tutorials/basic/python/
- https://grpc.io/docs/quickstart/python/
- https://github.com/grpc/grpc/tree/v1.25.0/examples/python/route_guide
- https://qiita.com/CyLomw/items/9aa4551bd6bb9c0818b6

生成する部分のツールチェインのほうが大切。

ちょくちょく古いけどまぁ直せる範囲。protocol bufferの知識があんまりないかもな。

## handofcats loggingを有効にするかどうか

- 長くなりすぎるので避けたいかも。

## pythonでgoogle authを使う方法の整理

- どこでtokenを得るか

  - oauth2.0 client id
  - service account

- access token
- token info
- revoke

- google-authパッケージの使い方
- requests越しに使う方法
- oauth2clientはオワコン

### misc

- flask-dance
- loginpass

そしてauthlib
