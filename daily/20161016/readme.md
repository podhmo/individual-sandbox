# python pymongo + 制限されたdict

[前回](../20161015/readme.md) からの進捗

## 進捗2

collections.UserListが使えないことがわかった。
詳しくは[ここ](../20161015/example_pymongo/03about_extend_encoder.md)

まじめにC拡張いじらないとダメ。

## 進捗3

- [mongodb/mongo-python-driver: PyMongo - the Python driver for MongoDB](https://github.com/mongodb/mongo-python-driver)

forkしてcollections.Sequenceを特別扱いするようにした。久しぶりにC書いたし。GOTOを使ったので悪。

# python mod使わずにfizzbuzz

cycle使うのが好き。

```python
import itertools


def fizzbuzz(begin, end):
    xs = [lambda i: print(i) for _ in range(15)]
    xs[3 - 1] = lambda i: print("fizz")
    xs[5 - 1] = lambda i: print("buzz")
    xs[15 - 1] = lambda i: print("fizzbuzz")
    it = itertools.cycle(xs)
    for i in range(begin, end + 1):
        next(it)(i)

if __name__ == "__main__":
    fizzbuzz(1, 100)
```
