# python そういえば、`__init__()` を迂回してオブジェクト生成したい場合もあるかも。

```python
x = X.__new__(X)
x.__dict__.update(d)
```

それなりに良い？参考は https://github.com/Dowwie/marshmallow_recipes/blob/master/people_zipcodes.py
