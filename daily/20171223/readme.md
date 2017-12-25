## pipのrequestを覗く

そもそも外にアクセスしているのは何があるんだろ？

:feet: pip/download.pyのPipSessionが使われているのが主っぽい。

`_make_session()`を探せば良い

```
grep -rl build_session . | grep -v pycache
./basecommand.py
./commands/wheel.py
./commands/download.py
./commands/search.py
./commands/list.py
./commands/uninstall.py
./commands/install.py
```

basecommandは毎回のコマンドのたびに新しいpipがないか調べてる。

searchとlistとdownloadあたりを見れば良いのか。


## python keyの衝突チェック(yaml,json)

### yaml

```python
import yaml


def construct_dict(loader, node):
    pairs = loader.construct_pairs(node)
    assert len(pairs) == set(pair[0] for pair in pairs)
    return dict(pairs)


yaml.add_constructor('tag:yaml.org,2002:map', construct_dict)
```

### json

```python
import json


def construct_dict(pairs):
    print(pairs)
    assert len(pairs) == set(pair[0] for pair in pairs)
    return dict(pairs)


with open("conflict.json") as rf:
    d = json.load(rf, object_pairs_hook=construct_dict)
```
