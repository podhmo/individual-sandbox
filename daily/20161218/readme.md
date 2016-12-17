# python *argsのあとにキーワード引数を付けられる

python3ならできるし。python2.x未サポートで良いや。

```python
class Handler(object):
    def __init__(self, *args, bar=None, **kwargs):
        print(args, kwargs)
        self.bar = bar


Handler(1, foo=2, bar=[])
# (1,) {'foo': 2}
```
