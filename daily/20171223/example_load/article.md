## はじめに

通常のjsonやyaml(PyYAML)のloadでは、元となる設定ファイルのキーに重複があった場合には後にあった値で上書きされる。エラーにならない。

例えば以下のようなJSONのnameは`"bar"`になる

conflict.json

```json
{
  "name": "foo",
  "age": 20,
  "name": "bar"
}
```

## jsonの場合

`json.load()`の`object_pairs_hook`にdictを構成する関数を自分で定義して渡す。このhookに設定された関数に渡される値は以下の様なkey-valueのペアのリストなのでここで重複判定してあげる。

```
[('name', 'foo'), ('age', 20), ('name', 'bar')]
```

以下のように。

```python
import json


def construct_dict(pairs):
    print(pairs)
    assert len(pairs) == set(pair[0] for pair in pairs)
    return dict(pairs)


with open("conflict.json") as rf:
    d = json.load(rf, object_pairs_hook=construct_dict)

print(d)
```

## yamlの場合

yamlも同様のhookを持っている。なので同様の方法で対応できる。

```python
import yaml


def construct_dict(loader, node):
    pairs = loader.construct_pairs(node)
    assert len(pairs) == set(pair[0] for pair in pairs)
    return dict(pairs)


yaml.add_constructor('tag:yaml.org,2002:map', construct_dict)

with open("conflict.yaml") as rf:
    d = yaml.load(rf)
print(d)
```

このときのconflict.yamlは以下。

```yaml
name: foo
age: 20
name: bar
```

## tomlの場合

tomlはデフォルトでキーの重複を許さない
