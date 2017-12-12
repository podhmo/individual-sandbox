#[python][json][yaml][toml][csv]pythonを使ってJSONと互換性のある(?)フォーマットを比べてみる

この記事はwaculアドベントカレンダーのN日目の記事です。今回は元のアドベントカレンダーの趣旨に従った軽い話です。

## JSONと互換性のあるフォーマットって？

### serialization/desrialization

JSONをserialization/serializationのためのフォーマットと見なしてみることにします。少なくともdict,list,float,int,str,boolの範囲では、ファイルに書き出して保存し、ファイルから読み込んで復元することができそうです(pythonの界隈では大抵の場合それぞれload,dumpという関数を用意しておくことが一般的です)。

例えば、こういうdictがあった場合に、

```python
d = {
    "person": "name",
    "age": 20,
}
```


元のdictとdictをファイルに書き出した(serialization)した後にファイルから読み込んだ(deserialization)dictの値は同じになります。

```python
import json

# serialization
with open("data.json", "w") as wf:
    json.dump(d, wf)

# deserialization
with open("data.json", "r") as rf:
    d2 = json.load(rf)

assert d == d2  # True
```



JSONではlistもdictも出力できます。コメントは書けません。

日本語を出力したい場合にasciiでescapeされてほしくない場合は`ensure_ascii=False`を付けてみてください。

```python
import json
import sys

d = {"name": "何か"}

json.dump(d, sys.stdout)
# {"name": "\u4f55\u304b"}

json.dump(d, sys.stdout, ensure_ascii=False)
# {"name": "何か"}
```

### 今回の話題

今回のテーマはJSONモジュールをベースにして他のフォーマットでdictを保存してみた時に何か違いがあるか調べてみようという話しです。あんまり複雑な話しはしないです。

## 準備

もし試すのであれば、以下のパッケージを使うので `pip install -r requirements.txt` とかしてインストールしておいて下さい。

requirements.txt

```
PyYAML
toml
```

## YAMLとの比較

まずはじめに比較するフォーマットはYAMLです。YAMLはJSONのスーパーセットなので実はJSONで保存されたファイルも全てvalidなYAMLファイルです。なのであまり意味は無いですが。JSONファイルをYAMLとして読み込むことができます。

data.json

```json
{"person": "name", "age": 20}
```


JSONはvalidなYAMLです。

```python
import yaml


with open("data.json", "r") as rf:
    d = yaml.load(rf)

print(d)
# {'age': 20, 'person': 'name'}
```

YAMLをdumpするときには2つのスタイルが存在しています。flowスタイルとblockスタイルです(ここでflowスタイルという言葉がでてきたのでもう少しJSONとの関係について詳しくいうとJSONはvalidなflowスタイルのYAMLです)。PyYAMLでは `default_flow_style` というオプションを使ってどちらのスタイルで出力するかを決める事ができます。

```python
import yaml
import sys


with open("data.json", "r") as rf:
    d = yaml.load(rf)

yaml.dump(d, sys.stdout, default_flow_style=False)
```

`default_flow_style=False` なので blockスタイルで出力されます。

```yaml
age: 20
person: name
```

yamlもlistでもdictでも出力できます(そしてコメントも書けます(コメントが書けるという話からJSON5などにも言及もするかというとしません))

list.yaml

```yaml
# リストとして評価される
- person: foo
  age: 20
```

コメント付きのyamlもふつうにloadできます。

```python
import yaml

with open("list.yaml") as rf:
    L = yaml.load(rf)


print(L)
# [{'age': 20, 'person': 'foo'}]
```

#### YAMLとJSONの互換性

YAMLとJSONの違いです。YAMLもJSONもそれぞれのフォーマット単体で使った場合にはdeserializeの結果が同じになりますが。YAMLとJSONを同時に使った場合に思わぬ差異が発生してしまう事があります。


以下のようなAPIレスポンスを模したdictがあるとします。これをファイルに書き込んだ後読み込んだ結果を比較すると等しくなりません(例はOpenAPI(swagger)documentの簡略版です)。

```python
d = {
    "/": {
        200: {
            "description": "ok response"
        },
        "default": {
            "description": "default response"
        },
    },
}
```

以下のようにそれぞれYAMLとJSONでファイルに出力した後ファイルから読み込んだ時の結果が一致しません。

```python
import json
import yaml

with open("schema.yaml", "w") as wf:
    yaml.dump(d, wf)
with open("schema.yaml", "r") as rf:
    from_yaml = yaml.load(rf)

with open("schema.json", "w") as wf:
    json.dump(d, wf)
with open("schema.json", "r") as rf:
    from_json = json.load(rf)

assert from_yaml == from_json  # AssertionError
```

原因はキーとなる値の扱い方です。JSONの仕様的にキーは文字列として決まっているのでdump後loadしたとき`200`というintは`"200"`というstrに変わっています。一方でYAMLはint型にも対応しているので`200`というintはintのままです(もちろんpickleなどでserialize/deserializeした場合も同様です)。swaggerなどのツールを使っている時にたまにハマリます。

schema.yaml

```yaml
/:
  200: {description: ok response}
  default: {description: default response}
```

schema.json

```json
{"/": {"200": {"description": "ok response"}, "default": {"description": "default response"}}}
```

### TOMLとの比較

次はTOMLです。TOMLは「Tom's Obvious, Minimal Language」というわりとふざけた名前のミニ言語です。YAMLが複雑過ぎるということに対するカウンターカルチャーという感じで生まれました。


TOMLもJSONと同様にserialize/deserializeできます。serializeするときのキーの表現はJSONと同様です。strのみのようです。というよりpythonのtomlパッケージの場合にはわざわざstrに変換してくれることもなくぶっきらぼうにエラーを返します(ただしこれはTOML自体の仕様というよりも今利用しているpythonのライブラリの実装によるところだと思います)。

```python
import toml


d = {
    "/": {
        200: {
            "description": "ok response"
        },
        "default": {
            "description": "default response"
        },
    },
}

with open("schema.toml", "w") as wf:
    toml.dump(d, wf)
# TypeError: expected string or bytes-like object
```


`200`を文字列に変えてdumpしてあげると以下の様な形で出力されます。

schema.toml

```toml
["/".default]
description = "default response"

["/".200]
description = "ok response"
```

TOMLは主に設定ファイルなどに使われます。コメントもかけます。

data.toml

```toml
# ここにコメント
name = "foo"
age = 20
```

load.py

```python
import toml

with open("data.toml") as rf:
    d = toml.load(rf)

print(d)
# {'name': 'foo', 'age': 20}
```

### TOMLとJSONの互換性

TOMLもJSONと似たような形で利用することができさらにコメントも書けます。シンプルで良いですが微妙に違いもあります。それはリストを扱うことができないことです。正確にいうと`Dict[List]`のようなものは扱えますが`List[Dict]`のようなものを扱えません。

```
import toml

L = [
    {
        "person": "foo",
        "age": 20
    },
    {
        "person": "bar",
        "age": 10
    },
]

with open("people.toml", "w") as wf:
    toml.dump(L, wf)

# TypeError: expected string or bytes-like object
```

最も外側がdictであるならば大丈夫です。

```diff
--- 03list.py	2017-12-11 22:18:38.000000000 +0900
+++ 04dict.py	2017-12-11 22:23:09.000000000 +0900
@@ -11,5 +11,7 @@
     },
 ]
 
+d = {"people": L}
+
 with open("people.toml", "w") as wf:
-    toml.dump(L, wf)
+    toml.dump(d, wf)
```

people.toml

```toml
[[people]]
person = "foo"
age = 20

[[people]]
person = "bar"
age = 10
```

### CSV(TSV)との比較


### :warning: JSONとYAMLはkeyの衝突を許す、tomlはエラーになる。
