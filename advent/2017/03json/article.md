#[python][json][yaml][toml][csv]pythonを使ってJSONと互換性のある(?)フォーマットを比べてみる

この記事はwaculアドベントカレンダーのN日目の記事です。今回は元のアドベントカレンダーの趣旨に従った軽い話です。

## JSONと互換性のあるフォーマットって？

### serialization/desrialization

JSONをserialization/serializationのためのフォーマットと見なしてみることにします。少なくともdict,list,float,int,str,boolの範囲では、ファイルに書き出して保存し、ファイルから読み込んで復元することができそうです。

この記事では、python上のオブジェクトをファイルに書き出すことをserializationと呼び、逆にファイルからpythonオブジェクトとして読み込むことをdeserializationと呼ぶことにします(pythonの界隈では大抵の場合それぞれdumpとloadという関数を用意しておくことが一般的です)。


例えば、以下のようなdictがあったとします。

```python
d = {
    "person": "name",
    "age": 20,
}
```

### 元のobjectとdeserializeしたオブジェクトは同じもの

この元のdictとdictをファイルに書き出した(serialization)した後にファイルから読み込んだ(deserialization)dictの値は同じになります。

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

このような振る舞いを持つモジュールのそれぞれを比べてみようというのがこの記事の趣旨です。

## 準備

今回の記事で利用する外部パッケージです。もし手元でも試したいという人がいれば、以下のパッケージを使うので `pip install -r requirements.txt` などでインストールしておいて下さい。

requirements.txt

```
PyYAML
toml
```

## JSON

はじめは[JSON](https://www.json.org/)についてです。

pythonは標準ライブラリに[json](https://docs.python.org/3/library/json.html)モジュールを持っています。

この種の常として、順序を保持したいだとか、インデントがどうこうとか、pretty printだとか、日本語(マルチバイト文字)の扱いなどについての知見について調べたいということがあったりするかもですが、今回の記事ではそれらについて一切触れません。他に良い記事があるので探してみてください。

### JSONは当然listもdictも出力可能

JSONではlistもdictも出力できます。当たり前のことかもしれないですが。

```python
# dict
person = {"name": "foo", "age": 20}
print(json.dumps(person)) # {"age": 20, "name": "foo"}

# list
print(json.dumps([person])) # [{"age": 20, "name": "foo"}]
```

### JSONにはコメントは存在しない

JSONには、通常は、コメントが存在しません(通常はコメントが存在しません)。

```json
// ここはコメント(ということはできない)
{
  "name": "foo",
  "age": 20
}
```

### JSONは重複したキーが有る場合には後のものが優先

重複したキーをJSONファイルにエラーになりません。仕様には明記されていないですが。[路線図(rail load graph)](https://www.json.org/)を見る限り、objectの構造は`"<string>" ":" "<value>"`の繰り返しなので後勝ちになりそうです。実際、pythonでも重複したキーを保持したJSONを渡してもエラーにはなりません。

```python
s = """
{
    "name": "foo",
    "age": 20,
    "name": "bar"
}
"""
print(json.loads(s))  # {'name': 'bar', 'age': 20}
```

## JSONとYAMLの比較

はじめにJSONと比較するフォーマットは[YAML](http://yaml.org/)です。YAMLはJSONのスーパーセットなので実はJSONで保存されたファイルも全てvalidなYAMLファイルです。なのであまり意味は無いですが。JSONファイルをYAMLとして読み込むことができます。

data.json

```json
{"person": "name", "age": 20}
```


JSONはvalidなYAMLです。JSONファイルをYAMLとして読み込むことができます。

```python
import yaml


with open("data.json", "r") as rf:
    d = yaml.load(rf)

print(d)
# {'age': 20, 'person': 'name'}
```

YAMLをserializeするときには2つのスタイルが存在しています。flowスタイルとblockスタイルです。ここでflowスタイルという言葉がでてきたのでもう少しJSONとの関係について詳しくいうとJSONはvalidなflowスタイルのYAMLです。

PyYAMLでは `default_flow_style` というオプションを使ってどちらのスタイルで出力するかを決める事ができます。

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

### YAMLも当然listもdictも出力可能

YAMLもJSONと同様にlistもdictも出力できます。当たり前のことかもしれないですが。

```python
import yaml
import sys

# dict
person = {"name": "foo", "age": 20}
yaml.dump(person, sys.stdout, default_flow_style=False)
# age: 20
# name: foo

# list
yaml.dump([person], sys.stdout, default_flow_style=False)
# - age: 20
#   name: foo
```

### YAMLにもコメントが存在

YAMLにはコメントが存在しています。コメントが書けるという話からJSONのところで[JSON5](http://json5.org/)などに触れれば良かったのでは？という話しがあるかもしれませんが触れませんでした(タイトルにfor humanと付いているものは基本的に苦手)。

list.yaml

```yaml
# リストとして評価される
- person: foo
  age: 20
```

コメント付きのyamlもふつうにloadできます。

### YAMLとJSONの違いによって異なるオブジェクトと判断されることも

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

### JSONとTOMLの比較

次はTOMLです。TOMLは「Tom's Obvious, Minimal Language」というわりとふざけた名前のミニ言語です。YAMLが複雑過ぎるということに対するカウンターカルチャーという感じで生まれました。生まれたらしいです。

TOMLは主に設定ファイルなどに使われます。

### TOMLもオブジェクトのキーはstr

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


`200`を文字列に変えてdumpしてあげると以下の様な形で出力されます。まぁ読みやすくはないですが。特に複雑にネストした構造のものなどはあんまり読みやすくならない事が多いです。

schema.toml

```toml
["/".default]
description = "default response"

["/".200]
description = "ok response"
```

### TOMLにもコメントが存在

TOMLはコメントもかけます。設定に使う分にはシンプルで良いです。

data.toml

```toml
# ここにコメント
name = "foo"
age = 20
```

### TOMLも当然listもdictも出力可能(?)

JSONとTOMLには微妙に違いもあります。それはlistを扱うことができないことです。今まで当たり前ですがと前置きをしつこく繰り返してきたのはこのためです。TOMLには無理でした。

正確にいうと`Dict[List]`のようなものは扱えますが`List[Dict]`のようなものを扱えません。

```python
import toml

# listを出力できません。
L = [
    {
        "name": "foo",
        "age": 20
    },
    {
        "name": "bar",
        "age": 10
    },
]

with open("people.toml", "w") as wf:
    toml.dump(L, wf)

# TypeError: expected string or bytes-like object
```

もっとも外側がdictであるならば大丈夫です。つまり`List[Dict]`のようなものは無理ですが `Dict[List]`のようなものは可能です。

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
name = "foo"
age = 20

[[people]]
name = "bar"
age = 10
```

設定ファイルとして使う分には十分だと思います。

### TOMLは重複したキーを許さない

もう１つの違いは重複したキーの扱いです。TOMLは重複したキーの存在を許しません。これも設定ファイルとして使う分には良い振る舞いだと思います。

```toml
import toml

s = """
name = "foo"
age = 20
name = "bar"
"""

d = toml.loads(s)
# toml.TomlDecodeError: Duplicate keys!
```

許しません。

### JSONとCSV(TSV)との比較

今度は趣向を変えてCSVとの比較です。設定ファイルとしてのJSONとは異なるものの、特に複数の値を保持しておきたい場合などにはCSVなどが使われる事があります。APIのレスポンスとしてのJSONと似た位置ある場合があるかもしれません。

pythonの標準ライブラリに[csv](https://docs.python.org/3/library/csv.html)モジュールは存在しますが。残念ながらload,dumpの関数は持っていません。

people.csv

```csv
name,age
foo,20
bar,10
```

header付きのものはDictReader,DictWriterで扱うのが一番取り回しがききやすいです。

```python
import csv

with open("people.csv") as rf:
    L = list(csv.DictReader(rf))

print(L)
# [{'name': 'foo', 'age': '20'}, {'name': 'bar', 'age': '10'}]
```

ところで、python3.6からは通常のdictではなくcollections.OrderedDictが返ります。便利ですね。

```
# python3.6での結果
[OrderedDict([('name', 'foo'), ('age', '20')]), OrderedDict([('name', 'bar'), ('age', '10')])]
```

### CSVが許すのはlistだけ

CSVで格納できるフォーマットはlistだけです。ある意味一行だけ取り出すということにしてdictを保存する事はできないわけではないですが。それをするくらいなら他のフォーマットで保存した方が良いと思います。

### CSVで読み込んだものは全てstrに

CSVには型の指定が存在しません。なので当然全てstrになります。

```
[{'name': 'foo', 'age': '20'}, {'name': 'bar', 'age': '10'}]
```

全部文字列です。

### CSVでもキーの重複は関知されない

そもそもヘッダー無しのcsvは単に","で区切られただけのテキストですし。キーの重複も何も無いですね。一方ヘッダー付きのcsvの場合でも、キーの重複をcsvモジュールが教えてくれるということはありません。

conflict.cs

```csv
name,age,name
foo,20,bar
bar,10,boo
```

JSONと同じように後のものが勝ちます。

```python
import csv

with open("conflict.csv") as rf:
    L = list(csv.DictReader(rf))

print(L)
[{'age': '20', 'name': 'bar'}, {'age': '10', 'name': 'boo'}]
```


### CSVにはコメントがありません

CSVにはコメントがありません。残念。ただちょっとしたデータのハンドリングでたまにお世話になる[pandas](https://pandas.pydata.org/)というライブラリから読み込む場合にはコメントを指定することができます。

例えば "#"をコメントとして扱った以下のようなcsvも

```csv
# これはコメント。CSVとしてはinvalid
name,age
foo,20
bar,10
```

pandasからは読めます。親切ですね。

```python
import pandas as pd

L = pd.read_csv("with-comment.csv", comment="#")
print(L["age"])
#   name  age
# 0  foo   20
# 1  bar   10
```

### CSVは全部文字列..ただし

CSVは全部文字列と言ってましたが。先程のpandasというパッケージ、これは親切というかおせっかいというか気が効いているので良い感じにintっぽいものはint(floatっぽいものはfloat)など空気を読んでくれます。

```
L = pd.read_csv("with-comment.csv", comment="#")

print(L["age"])
# 0    20
# 1    10
# Name: age, dtype: int64
print(L["age"].mean())
# 15.0
```

これで完全に解決かというとそうでもなく例えばIDが"001"みたいなものの場合も数値として扱われたりします。

with-id.csv

```csv
id,name,age
0001,foo,20
0002,bar,10
```

本当はidが文字列になってほしいのですが。。pandasもpandasの実装をしてくれた人もエスパーではないので。。

```python
L = pd.read_csv("with-id.csv", comment="#")
print(L["id"])

# 0    1
# 1    2
# Name: id, dtype: int64
```

### (おまけ pickle)

python限定という話しであれば[pickle](https://docs.python.org/3/library/pickle.html)と言うものもありますね。

```python
import pickle
d = {"name": "foo", "age": 20}
assert d == pickle.loads(pickle.dumps(d))
```

pickleの良いところは、pythonの範囲で対応しているオブジェクトであればどのようなオブジェクトでもserializeが可能ということです。
pickleの悪いところは、利用可能な言語がpythonのみに限られるということです。

## おわりに
