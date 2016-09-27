# python

以下まとめておきたい

- 全テストの開始前と全テストが終了後に呼ぶ処理を付加する方法(setUpModuleなどよりも広いスコープ)
- requestsでしばらくsleepしつつretryする簡易的な書き方
- REST APIについてのrequestsの呼び方

# python subprocessを実行したpythonで呼ぶ。

以下ではまずいはず。

```python
subprocess.call(["python","x.py])
```

[sys.executable](https://docs.python.org/3/library/sys.html#sys.executable) がある。

# python nameとvalueの一覧が欲しい場合

普通にenumを使ったほうが楽そう。

```python
# -*- coding:utf-8 -*-
import enum


class S:
    waiting = "waiting"
    running = "running"
    done = "done"
    error = "error"
    cancelled = "cancelled"

print(list(k for k, v in S.__dict__.items() if k == v))


class S2(enum.Enum):
    waiting = "waiting"
    running = "running"
    done = "done"
    error = "error"
    cancelled = "cancelled"
print(S2.waiting.name)
print(S2.waiting.value)
print(S2.__members__)
```

# python mongo mongoengine

testにはmockもある?

- [2.9. Use mongomock for testing — MongoEngine 0.10.6 documentation](http://docs.mongoengine.org/guide/mongomock.html)

## とりあえずテスト用のdbとアプリ用のdbの読み込みを変える

接続するdbを変えることにしよう。

- [Flask-MongoEngine — Flask-MongoEngine 0.8 documentation](http://docs.mongoengine.org/projects/flask-mongoengine/en/latest/)

# python mongoengine signal使う

hmm?

```
RuntimeError: signalling support is unavailable because the blinker library is not installed.
```

```
pip install mongoengine blinker
```

## python connexion endpointの値を見るのをconnectionIdから別ものに変える

resolver書き換えれば良い。

```python
import connexion


class MyResolver(connexion.Resolver):
    def resolve_operation_id(self, operation):
        spec = operation.operation
        return spec.get("x-python-operationId")


def main():
    app = connexion.App(__name__)
    app.add_api("my_api.yaml", resolver=MyResolver())
    app.run(port=4040)


if __name__ == "__main__":
    main()
```
