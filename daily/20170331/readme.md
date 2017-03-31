# python メモリーの消費量見るやつ

- objgraph
- memory_profiler


http://mg.pov.lt/objgraph/

```python
import objgraph
_i+=1
print(_i)
objgraph.show_most_common_types()
```

```
pip install psutil objgraph memory_profiler
```

```
@profile
def main():
    # do something
    globals().update(locals())
main()
```

## 追記

tracemallocというのが3.4から追加されてる。

https://docs.python.jp/3/library/tracemalloc.html

