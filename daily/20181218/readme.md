## go mock

- vanila mock
- gomock
- testify/mock

### どういうコードが良いんだろう？

testdobuleで考えてみる

- dummy (使われないダミー)
- fake (より単純. e.g. inmemory db)
- stub (テスト対象への間接的な入力)
- spy (テスト対象からの出力を記録)
- mock (あらかじめ期待する値を設定しておく、テスト対象からの出力を実行時に検証する)

#### テスト対象への間接的な入力

```
def _callFUT(x, y):
    f(x,y)

def f(x, y):
    use(x.method())

def test():
    class stub:
        class method(self):
            return 42
    dummy = None
    _callFUT(stub, dummy)
```

stdinの上書きみたいなのも含まれる？

