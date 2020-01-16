## dataclasses with marshmallow

- https://github.com/lovasoa/marshmallow_dataclass/blob/master/marshmallow_dataclass/__init__.py

nestしたときにちょっと困る。dataclasses単体だと。

## python typing

以下の２つを使ったほうが良い？

- get_origin()
- get_args()

https://docs.python.org/3.8/library/typing.html#typing.get_origin

### runtime_checkable()

3.8からこんなものが入っていたんだ

## mypy reval_type

いつも忘れる。

- https://mypy.readthedocs.io/en/latest/common_issues.html#displaying-the-type-of-an-expression

## python context manager

この２つを同時に満たそうとするときcontextlib.ContextDecoratorが便利。

- context manager
- decorator

どうやって実装されている？

hmm.

### ifを一つのcontext managerで行う

無理そう。

- 条件が偽のときには処理をスキップしなければいけない。例外を送出。
- `__enter__` の中での例外はcontext managerの外側
- `__exit__` の中での例外もcontext managerの外側

そんなわけで頑張っても内側のcontext managerを外側で受け取る形にする必要がある

```python
block = partial(contextlib.suppress, Fail)
with block():
    with if_(cond):
        do_something() # ...
    # condがfalseのときにここも読み込まれない。
```

## 

## python dataclassの中でdataclass

- presetを定義したい
- frozen=Trueでもできたっけ？

## ファイル監視の良い感じのものが欲しい

- throttlingの機能が欲しい
- back-pressureの機能も欲しい
- サービスとして立ち上がって欲しい (sub-process を spawnではなく)

### 念頭にあるもの

- watchdog https://github.com/gorakhargosh/watchdog
- fsnotify https://github.com/fsnotify/fsnotify
- chokidar https://github.com/paulmillr/chokidar

ただ最初はmonitoring自体はpoolingでも良い気がしている

#### memo

fsnotifyは再帰的に監視をしてくれないらしい。

- https://github.com/tywkeene/go-fsevents

