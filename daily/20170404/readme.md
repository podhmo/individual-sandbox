# yapf python yapfのimportをいじりたい

```python
from foo import (
    x,
    y
)
```

には対応できるのだけれど。以下がだめ。

```python
from foo import (  # NOQA
    x,
    y
)
```
