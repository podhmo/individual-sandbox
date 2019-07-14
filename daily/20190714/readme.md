## python docstringを全部集める

- starlette
- fastapi
- requests
- marshmallow

## python インタプリタとか補完周り

- [cmd](https://docs.python.org/ja/3/library/cmd.html)
- [code](https://docs.python.org/ja/3/library/code.html)
- [iptyhon](https://ipython.readthedocs.io/en/stable/api/generated/IPython.core.completer.html)

ipythonってprompt-toolkitに依存してたっけ？。依存している。

### ipythonで自分でreplを作る方針

何が知りたいんだろう？

- 自由度
- custom completer
- multiline input support
- session logging
- preprocess的なfilterの追加

### 拡張の仕方

- https://ipython.readthedocs.io/en/stable/config/index.html

### 裏側の実装の話

- https://ipython.readthedocs.io/en/stable/development/how_ipython_works.html

kernel

- native kernel
- wrapper kernel

kernelの作り方

- https://ipython.readthedocs.io/en/stable/development/wrapperkernels.html

messageの仕様

- https://jupyter-client.readthedocs.io/en/latest/messaging.html#messaging

### ipythonの非同期対応

- https://ipython.readthedocs.io/en/stable/interactive/autoawait.html

```
% autoawait
```

### decoupled repl

- https://ipython.readthedocs.io/en/stable/overview.html#decoupled-two-process-model
- https://nbviewer.jupyter.org/github/ipython/ipython/blob/1.x/examples/notebooks/Frontend-Kernel%20Model.ipynb


## discord.py

- 通信が覗きたいな

## へーというもの

- [ipyparallel](https://ipyparallel.readthedocs.io/en/latest/)
- [fileinput](https://docs.python.org/ja/3/library/fileinput.html)
- [ipython notebook](https://github.com/ipython/ipython/wiki/Cookbook%3A-Index)

## skylark

なんで作られたのという話。

- https://blog.bazel.build/2017/03/21/design-of-skylark.html

言語仕様

- https://github.com/bazelbuild/starlark/blob/master/spec.md

starlark-goからhttp requestするやつ

- https://github.com/pcj/starlark-go-nethttp
