## python line_profiler まじめに中身を理解する

- https://github.com/rkern/line_profiler
- https://docs.python.org/ja/3/c-api/index.html

気になること

- 依存関係は
- 何がどこで定義している？
- どうやって%をだしているの？

### setup.pyを見る

install_requires

- ipython

build部分

- Cython.Distutils.build_ext() をimportしてる
- `_line_profiler.pyx` が使えたらそちらを、そうでない場合は `_line_profiler.c`

modules

- kernprof
- line_profiler

ext modules

distutils.extension.Extnsionを使って `_line_profiler`

- _line_profiler.pyx
- timers.c
- unset_trace.c

を利用しているっぽい。

entry points

- `kernprof=kernprof:main`

### その前に使用方法を整理

```console
$ kernprof -l <file>
$ python -m line_profiler <outfile>.lprof
```

### kernprofで出力される .lprofの構造

line_profilerはCではない？もしかして。

```
py_modules = ['line_profiler', 'kernprof']


setup(
...
    py_modules = py_modules,
)
```

