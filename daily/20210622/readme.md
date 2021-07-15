## python 例外の出力を変更

色々hookがあった記憶。


- https://docs.python.org/ja/3.8/library/sys.html#sys.excepthook
- https://docs.python.org/ja/3.8/library/sys.html#sys.unraisablehook
- https://docs.python.org/ja/3.8/library/sys.html#sys.audit
- https://docs.python.org/ja/3.8/library/sys.html#sys.settrace

excepthookの方が良いかもしれない?
それぞれの説明を真面目に読もう。

### excepthook

- https://docs.python.org/ja/3.8/library/sys.html#sys.excepthook
- threading.excepthook()も必要そう

unraisablehookとの違いがわかっていない。

