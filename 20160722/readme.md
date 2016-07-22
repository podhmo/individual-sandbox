# python3でsuper classのclassmethodをsuperだけを使って呼べたっけ？

普通に `super()` でよかった。

```python
class A:
    @classmethod
    def for_class(cls):
        print("A", cls)

class B(A):
    @classmethod
    def for_class(cls):
        super().for_class()
        print("B", cls)
```
