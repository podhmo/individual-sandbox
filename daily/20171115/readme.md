## python threading.concurrent

使い方 exampleに書いた


## python threadingの同期プリミティブ的なもの

- Lock
- RLock
- Condition
- Event
- Semaphore
- Barrier

使い分けムズくない？

### lockについて

- Lockはacquire()とrelease()を持つだけのオブジェクト
- RLockは(Reentrance Lock)つまり再入可能なロック再帰的な記述に使う

### semaphoreについて

- SemaphoreはN回Lockできるオブジェクト
- (SemaphoreはConditionとLockで作られる)
- (releaseしすぎる場合を避けるためにBoundedSemaphoreがある)

例えばconnection数での制御とかに使う

```python
sem = Semaphore(5)

with sem:
    do_something()
```

### conditionについて

複数のthreadで利用と提供を分ける時に使うっぽい。詳しい使い方は[ドキュメント](https://docs.python.jp/3/library/threading.html#condition-objects)を見たほうが早そう。

- wait
- notify, notify_all

基本的にはRLockでlockを獲得するっぽい。

```python
c = Condition()

# f
with c:
    c.wait_for(is_available_item)
    do_something()

with c:
    make_item()
    c.notify()
```

### eventについて

eventは何かの状態flag的なものを管理

- set() -> true
- clear() -> false
- wait() -> trueになるまでblock
- is_set()

queue.Queueの代わりになりそう？
(ConditionとLockで作られている)

### barrierについて

最初使い道よくわからなかった。

- wait
- reset

barrierというよりもmarkerに近いかもしれない。
対象の複数のthreadに対してコード上の特定の位置に達したかどうかを表したい。
