# PyYAMLでyamlをload/dumpする時の振る舞いを変えてみる方法とその詳細について

## はじめに

PyYAMLはpython用のyamlライブラリ。

- [PyYAML](https://pypi.python.org/pypi/PyYAML/3.12)

このライブラリを使ってyamlをload/dumpする時の振る舞いを変えてみようという話。

## 前提

- Representer -- dumpのタイミングでtagを付加するためのhook object
- Constructor -- loadのタイミングでpython表現を生成するためのhook object

ためしに以下２つを例にPyYAMLの振る舞いを変更して自分の好みの挙動をするように変えてみる。

- OrderedDictに対応する(他のyaml loaderでも読める形式で入出力する)
- jsonと互換性をもたせた表現で出力するようにするjsonと互換性をもたせた表現で出力するようにする

## OrderedDictに対応する(他のyaml loaderでも読める形式で入出力する)

OrderedDictに対応するためには以下のコードを追加すれば良い。

```python
def represent_odict(dumper, instance):
    return dumper.represent_mapping('tag:yaml.org,2002:map', instance.items())

yaml.add_representer(OrderedDict, represent_odict)

def construct_odict(loader, node):
    return OrderedDict(loader.construct_pairs(node))

yaml.add_constructor('tag:yaml.org,2002:map', construct_odict)
```

設定例は以下のようにqiitaにも存在している。

- [PyYAMLで順序を保持して読み込む - Qiita](http://qiita.com/tkyaji/items/842840fa090fb7c6a095)

今回はたまたまPyYAMLの中を覗いてみることになったので、上の設定の意味などの詳細をメモして見ることにした。

### 出力(yaml.dump)の話

Pythonオブジェクトをyamlとして文字列化する際に `yaml.dump()` を使う。
この `yaml.dump()` は内部では `Representer.represent()` を呼ぶ。
これがPythonオブジェクトをyaml内部のNodeオブジェクトに変換し、 `Serializer.serialize()` で文字列に変換される。

内部的には、各Pythonオブジェクトに対して `type()` を呼び出しその戻り値の型で処理内容を分岐している。
分岐の候補が存在しなかった場合には、そのオブジェクトのmroを辿り変換用の候補を探す。
(特に候補が見つからないのであれば、候補の探索の最終地点としてobjectにたどり着き、`represent_object()` が呼び出される)

PyYAMLの立場としては、OrderedDictとdictを意識して使い分けたいらしく、OrderedDict用にわざわざrepresentationの関数を設定している。

```python
Representer.add_representer(collections.OrderedDict,
        Representer.represent_ordered_dict)
```

そんなわけで以下のようなPythonオブジェクトは、

```python
d = OrderedDict()
d["a"] = 1
d["z"] = 2
```

以下のような出力になる。

```
!!python/object/apply:collections.OrderedDict
- - [a, 1]
  - [z, 2]
```

これを防ぐためには、`OrderedDict` に対して通常と同様の出力をするようにrepresenterの設定を書き換えてあげれば良い。
ただし、出力するタイミングで順序は保持するように出力する必要がある。

dictについてもOrderedDictと同様に、Representerの設定が存在する。内部的にはmap Nodeとして扱われている。

```python
SafeRepresenter.add_representer(dict,
        SafeRepresenter.represent_dict)

class SafeRepresenter:
    def represent_dict(self, data):
        return self.represent_mapping('tag:yaml.org,2002:map', data)
```

そんなわけでOrderedDictをmap nodeとして出力するためには以下を追加すれば良い。

```python
def represent_odict(dumper, instance):
    return dumper.represent_mapping('tag:yaml.org,2002:map', instance.items())

yaml.add_representer(OrderedDict, represent_odict)
```

### 入力(yaml.load)の話

load時も同様で、`yaml.load()` の内部では、`Constructor.get_single_data()` が呼ばれる。
こちらも、一度 `get_single_node()` でNodeオブジェクトを生成し、 `construct_document()` でPythonオブジェクトに変換する。

今度も同様で各Nodeに対する変換の対応を付けてあげれば良い。Nodeオブジェクト自体は以下の様な定義のオブジェクトになっている。

```python
class Node(object):
    def __init__(self, tag, value, start_mark, end_mark):
        self.tag = tag
        self.value = value
        self.start_mark = start_mark
        self.end_mark = end_mark
```

このtagの部分でどのように処理を行うか決めている。

先の対応でOrderedDictであるかないかにかかわらず、map(dictとして扱われる)のtag付きのnodeとして保持されることになる。
なのでmapのtag付きのnodeに対する変換を設定してあげれば良い。

ところで、yamlにはpairsと言うものが実は存在していて、こちらが順序を保持することに都合が良い。
これを経由してOrderedDictを作るのが手軽。

pairsのタグ指定を行なったものは、以下の様に変換される(pairの意味の通り長さ2のtuple)。

```
s = """\
foo:
  !!pairs
  - a: b
  - x: y
"""
load(s)  # {'foo': [('a', 'b'), ('x', 'y')]
```

このpairsを使って以下のような設定を追加する。

```python
def construct_odict(loader, node):
    return OrderedDict(loader.construct_pairs(node))

yaml.add_constructor('tag:yaml.org,2002:map', construct_odict)
```

## jsonと互換性をもたせた表現で出力するようにする(dictの数値のkeyを文字列にする)

jsonで設定ファイルを管理するのはコメントが書けないなどで面倒ということがある。
そのような場合にyamlで書くこともある。特にjsonschemaやswaggerなどの設定をjsonで書くのは辛いのでyamlを使う事がある。

大抵の場合は問題が起きないのだけれど。keyが数値のdictの表現でjsonとyamlとの間に差異があるので辛いと言うことがある。
具体的には以下のような状況。

このようなjsonschemaがあるとする。

```javascript
{
    "type": "object",
    "patternProperties": {
        "\d{3}": {"type": "string"}
    },
    "additionalProperties": False
}
```

このschemaにマッチする値として以下の様なdictを考えた時にちょっとした不都合が生じる(不都合であって不適切ではない)。
こちらのコードはvalidになるのだけれど。これをyamlで行うとinvalidになってしまう。

```python
import json

validate(json.loads(json.dumps({200: "ok"})), schema)
```

```python
import yaml
from io import StringIO

# yamlの方でもjson moduleと同様にdumps,loadsを定義しておいて欲しい。。

def loads(s):
    io = StringIO(s)
    return yaml.load(io)


def dumps(d):
    io = StringIO()
    yaml.dump(d, io)
    return io.getvalue()

validate(loads(dumps({200: "ok"})), schema)  # error
```

これは何故かと言うと、上のdictのデータがyaml上では以下のように表現されるせい。

```yaml
200: ok
```

上のyamlを少し冗長で正確に記述すると以下の様なものになる。yaml上ではmap Nodeのkeyの型が数値であっても数値のまま。

```yaml
!!int 200: ok

# こう書けば {'200': ok} として認識される
'200': ok

# あるいは こう書けば {'200': ok} として認識される
!!str 200: ok
```

json上ではオブジェクトのkeyとしては文字列のみを許容しているので自動的に文字列として扱われる。
この挙動に合わせた設定を追加しようという話。これは今までの復習みたいなもので以下のような設定を加えれば良い。

```python
def construct_json_compatible_map(loader, node):
    return {str(k): v for k, v in loader.construct_pairs(node)}

yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG  # 'tag:yaml.org,2002:map'
yaml.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, construct_json_compatible_map)
```
