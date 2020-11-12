## python httpx with schema validation

- serverでのaccessorは色んなsourceからdictを作る
- clientでのaccessorは？

  - dictのvalidation
  - dictの箇所を色んなsourceに割り当てる -> make request

## python sys.audit

- https://docs.python.org/ja/3/library/sys.html#auditing
- https://www.python.org/dev/peps/pep-0578/
- https://www.python.org/dev/peps/pep-0551/

### misc

- https://daddycocoaman.dev/posts/bypassing-python38-audit-hooks-part-1/

## python 例外の発生箇所でpdb

- https://www.oreilly.com/library/view/python-cookbook/0596001673/ch14s06.html
- https://docs.python.org/ja/3/library/sys.html#sys.excepthook

どういう仕組み？

- sys.excepthookで補足されない例外の取扱いを決められる
- pdbでpost mortem modeで起動する

```py
def post_mortem(t=None):
    # handling the default
    if t is None:
        # sys.exc_info() returns (type, value, traceback) if an exception is
        # being handled, otherwise it returns None
        t = sys.exc_info()[2]
    if t is None:
        raise ValueError("A valid traceback must be passed if no "
                         "exception is being handled")

    p = Pdb()
    p.reset()
    p.interaction(None, t)
```

キモはreset() + interraction()か。そもそも通常の`set_trace()`では何が動くんだろう？

```py
def set_trace(*, header=None):
    pdb = Pdb()
    if header is not None:
        pdb.message(header)
    pdb.set_trace(sys._getframe().f_back)
```

ところでpdb:Pdbのinspectはこんな感じ[pdb.inspect](./pdb.inspect)

```
    [method] set_trace(self, frame=None)
        [method] reset(self)
            [method] _set_stopinfo(self, stopframe, returnframe, stoplineno=0)
        [method] trace_dispatch(self, frame, event, arg)
            [method] dispatch_line(self, frame)
                [method] stop_here(self, frame)
                    [method] is_skipped_module(self, module_name)
                [method] break_here(self, frame)
                    [method] canonic(self, filename)
                    [method] do_clear(self, arg)
                [method] user_line(self, frame)
            [method] dispatch_call(self, frame, arg)
                [method] user_call(self, frame, argument_list)
                [method] stop_here(self, frame)
                    [method] is_skipped_module(self, module_name)
                [method] break_anywhere(self, frame)
                    [method] canonic(self, filename)
            [method] dispatch_return(self, frame, arg)
                [method] stop_here(self, frame)
                    [method] is_skipped_module(self, module_name)
                [method] user_return(self, frame, return_value)
                [method] _set_stopinfo(self, stopframe, returnframe, stoplineno=0)
            [method] dispatch_exception(self, frame, arg)
                [method] stop_here(self, frame)
                    [method] is_skipped_module(self, module_name)
                [method] user_exception(self, frame, exc_info)
        [method] set_step(self)
            [method] _set_stopinfo(self, stopframe, returnframe, stoplineno=0)
            [method] trace_dispatch(self, frame, event, arg)
                [method] dispatch_line(self, frame)
                    [method] stop_here(self, frame)
                        [method] is_skipped_module(self, module_name)
                    [method] break_here(self, frame)
                        [method] canonic(self, filename)
                        [method] do_clear(self, arg)
                    [method] user_line(self, frame)
                [method] dispatch_call(self, frame, arg)
                    [method] user_call(self, frame, argument_list)
                    [method] stop_here(self, frame)
                        [method] is_skipped_module(self, module_name)
                    [method] break_anywhere(self, frame)
                        [method] canonic(self, filename)
                [method] dispatch_return(self, frame, arg)
                    [method] stop_here(self, frame)
                        [method] is_skipped_module(self, module_name)
                    [method] user_return(self, frame, return_value)
                    [method] _set_stopinfo(self, stopframe, returnframe, stoplineno=0)
                [method] dispatch_exception(self, frame, arg)
                    [method] stop_here(self, frame)
                        [method] is_skipped_module(self, module_name)
                    [method] user_exception(self, frame, exc_info)
```


### misc

- https://www.python.org/dev/peps/pep-0553/
- https://docs.python.org/ja/3/library/sys.html#sys.unraisablehook

みたいな感じで環境変数経由でしかけられると便利。

## python httpx

そういえば、httpxでのあれこれを書いたことがなかったな。
httpclientを生成したいということについてのあれこれで。

必要なものは大体揃っているような気がする。

### client

- https://www.python-httpx.org/advanced/

### create request

- https://www.python-httpx.org/advanced/#request-instances

### custom transport

- https://www.python-httpx.org/advanced/#writing-custom-transports

### 追記

へー、httpxってwsgi appを直接渡して呼べちゃうんだ。便利かも。
