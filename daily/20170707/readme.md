## python memory

- [27.7. tracemalloc — メモリ割り当ての追跡 — Python 3.6.1 ドキュメント](https://docs.python.jp/3/library/tracemalloc.html)
- [Diagnosing and Fixing Memory Leaks in Python | Fugue](https://blog.fugue.co/2017-03-06-diagnosing-and-fixing-memory-leaks-in-python.html)

メモリーのやつどうすれば良いんだっけ？

### linecache

linecache知らなかった


### tracemalloc

snapshotした結果のstaticsの内容

|名前|意味|
|:-|:-||
|traceback|traceback|
|size|total size(byte)|
|count|メモリーブロック数|

snapshot取るのはこういう感じ。

```python
tracemalloc.start()

# do something

snapshot = tracemalloc.take_snapshot()
snapshot = snapshot.filter_traces(
    (
        tracemalloc.Filter(False, "<frozen importlib._bootstrap>"),
        tracemalloc.Filter(False, "<unknown>"),
    )
)
limit = 10
top_stats = snapshot.statistics("traceback")
for i, stat in enumerate(top_stats[:limit], 1):
    do(stat)
```

traceback全部表示は以下のような感じ。

```python
for line in stat.traceback.format():
    print(line)
```

### 定期的なメモリーサイズを表示

```python
import tracemalloc as t
t.start()

print([t._format_size(x, False) for x in t.get_traced_memory()])
```

こちらのサイズは合う。

hmm sizeが合わない。

`-X tracemalloc`
