# mongodb かえってきたデータ整形する

```
> db.<collection>.find().pretty()
```

# python yaml.loadがsyntax error返さない？

例: Noneになる

```python
import yaml
from io import StringIO

s = """
foo:

bar:
 x: y
"""

data = yaml.load(StringIO(s))
print(data)  # {'foo': None, 'bar': {'x': 'y'}}
```

