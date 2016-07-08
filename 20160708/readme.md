# unittest.mockの `m.call_assert_with()` を騙すdummy objectの作り方

```python
class Dummy:
    def __eq__(self, other):
        return True
```

結局、 `call_assert_with()` は以下の様なことをしているに過ぎないので。

```python
# unittest/mock.py

    def assert_called_with(_mock_self, *args, **kwargs):
        # .. snip
        expected = self._call_matcher((args, kwargs))
        actual = self._call_matcher(self.call_args)
        if expected != actual:
            # AssertionError送出
```
