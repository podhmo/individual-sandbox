# python pytestまた再開してみよう

```
$ pip install pytest
```

## fixtureのscopeの数幾つあるんだっけ？

- module
- session
- function

fixtureを使う時は、引数とデコレーターを与える関数の名前を一緒にしないとダメっぽい

```python
import pytest

@pytest.fixture("module")
def m():
    return object()

def test_x(m):
    return
```

あと、setup teardown的なもの

```python
# content of conftest.py
@pytest.fixture
def transact(self, request, db):
    db.begin()
    yield
    db.rollback()
```

```python
@pytest.mark.usefixtures("transact")
class TestClass:
    def test_method1(self):
```

## importできる感じにするには

conftestになんか書いとくっぽい。

## markerを使う方法を把握しておきたい

- [Installation and Getting Started — pytest documentation](http://doc.pytest.org/en/latest/getting-started.html#grouping-multiple-tests-in-a-class)
- `-k` が便利？
- `-s` captureしない
- `--tb=no`
# python modulegraphは何するライブラリ？

- [modulegraph 0.12.1 : Python Package Index](https://pypi.python.org/pypi/modulegraph/)


