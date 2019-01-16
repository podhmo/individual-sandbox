## 作業

- 不特定回のasyncio taskをつくる(無限worker)
- 不特定回のasyncio taskをつくる(有限worker)

## python jediでresource warning

## python jediの実行結果調べる

昔やっていなかったっけ？
[../20180704/example_jedi/00usejedi](../20180704/example_jedi/00usejedi)

```
sys:1: ResourceWarning: unclosed file <_io.BufferedWriter name=4>
sys:1: ResourceWarning: unclosed file <_io.BufferedReader name=5>
```

以下に変える。

```python
            with self._process:
                self._process.kill()
```

なんかエラーが出る。

```
Fatal Python error: could not acquire lock for <_io.BufferedReader name=7> at interpreter shutdown, possibly due to daemon threads
```

これのせい

```python
        self._stderr_queue = Queue()
        self._stderr_thread = t = Thread(
            target=_enqueue_output,
            args=(process.stderr, self._stderr_queue)
        )
        t.daemon = True
        t.start()
```

ここで使われている感。

```python
def _enqueue_output(out, queue):
    for line in iter(out.readline, b''):
        queue.put(line)
    out.close()
```
