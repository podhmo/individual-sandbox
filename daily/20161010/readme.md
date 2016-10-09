# python logging 常に特定の引数を付加したい

例えば asctime(現在時刻) の代わりに経過時刻にしたい。

log record factoryを変更するのが楽そう

```python
class Extension:
    def __init__(self):
        self.c = defaultdict(int)


class LogRecordExtension(logging.LogRecord):
    extension = Extension()

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.extension.c[self.msg] += 1
        self.call_count = self.extension.c[self.msg]


logging.setLogRecordFactory(LogRecordExtension)
```
