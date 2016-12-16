# python 最近良く使うdictのwalker

最近よくこういうwalkerを使う


```python

def on_container(path, d):
    print(path)
    print(d)

walker = LooseDictWalker(["b", "e"], {"a": {"b": {"c": {"d": {"e": 10}}}}})

# ["a", "b", "c", "d", "e"]
# {"e": 10}
```

詳しい話はコチラ [./example_dict](./example_dict)
