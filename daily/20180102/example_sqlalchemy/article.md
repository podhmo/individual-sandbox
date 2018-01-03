#[python][sqlalchemy][dictknife][alchemyjsonschema]新年最初のコードはalchemyjsonschemaを1年ぶり位に弄ることだった

新年最初のコードは[alchemyjsonschema](https://github.com/podhmo/alchemyjsonschema)を1年ぶり位に弄ることだった。

## alchemyjsonschema

sqlalchemyのmodelの定義から対応する感じのjsonschemaを生成するコマンド(ライブラリ)。去年くらいにjsonschemaではなくswaggerをdefaultにした。

## やったこと

やったことは以下

- formatオプションでjsonとyamlの出力を分けられるようにした
- layoutオプションでswagger2.0以外の表現で出力できるようにした

これに付随して以下の作業をした。結構依存した作業が多かった

- alchemyjsonschemaでも[dictknife](https://github.com/podhmo/dictknife)を使うようにした
- alchemyjsonschemaでも[magicalimport](https://github.com/podhmo/magicalimport)を使うようにした
- ciの適用範囲を3.5だけから3.4,3.5,3.6,3.6-dev,nightlyに変更した
- dictknifeでjson,yamlを出力する時に`--sort-keys`オプションを使えるようにした。

### dictkinfeを使うようにした

もともとalchemyjsonschemaでは出力がjsonに限定されていた。去年の9月頃に`--swagger`とか指定したときにyamlになっていると嬉しいみたいな[issue](https://github.com/podhmo/alchemyjsonschema/issues/13)が作られていて反応していなかったのだけれど。まぁ暇つぶしにやってみるかということでサポートする気になった。

とは言えswaggerならという表現はひどく曖昧で(そもそもOAS2.0とOAS3.0どっちを指すんだとか。現在は過渡期なのだけれど。多くのツールがまだ2.0だとか)それを指定するよりは明示的にファイルフォーマットを指定できた方が良いだろということでformatオプションを作るようにした。

```shell
# defaultはformat=json
$ alchemyjsonschema models.py
# yamlが欲しい場合にはformat=yaml
$ alchemyjsonschema models.py --format=yaml
```

この対応はdictknifeを使うとけっこう手軽にできる。dictknifeは個人的に作っているライブラリで色々な自作のツールがこれに依存し始めてしまっている(便利なのだけれど代替品があるならそちらを使いたいという気持ちもある)。

良いところは2つあって。

- 出力の形式を`format`オプションで受け取れる様になっていること
- 「標準出力あるいはファイル出力」みたいな処理の分岐のコードが不要になること

#### 出力の形式を`format`オプションで受け取れる様になっていること

1つは出力の形式を`format`オプションで受け取れる様になっていること(加えて一方を指定した場合にもう一方はimportされない(例えばjsonを指定して実行したときにyaml用の依存ライブラリを無駄にimportしない))。

```python
from dictknife import loading

d = {"name": "foo"}

with open("person.json", "w") as wf:
    loading.dump(d, wf, format="json")

# 実は拡張子を見るので以下でもOK
with open("person.yaml", "w") as wf:
    loading.dump(d, wf)
```

defaultはyamlなのだけれど。`loading.setup`で変えられる。

```python
# jsonをdefaultに
loading.setup(loading.json.load, loading.json.dump)
```

#### 「標準出力あるいはファイル出力」みたいな処理の分岐のコードが不要になること

もう１つの良いところは「標準出力あるいはファイル出力」みたいな処理の分岐のコードが不要になっているところ。元々のalchemyjsonschemaのコードでもオプションに`--out`を指定するとファイル出力。ない場合には標準出力に出力という形になっていたのだけれど。

```shell
# output to stdout
$ alchemyjsonschema models.py
# output to schema.json
$ alchemyjsonschema --out models.py
```

このコードに対応するために以下のような分岐があった。

```python
    if args.out:
        with open(args.target, "w") as wf:
            return driver.run(args.target, wf)
    else:
        return driver.run(args.target, sys.stdout)
```

これが地味にだるいのだけれど。最近の自分のコードではdictknifeを使って以下の様に書ける。

```python
    driver.run(args.target, args.out)
    # 内部的には loading.dumpfile(args.target, args.out) みたいな形になっている
```

渡されたportがNoneのときには標準出力に出力するというコードになっているので。argparseでdefaultを指定しておくと良い感じ。

```python
import argparse
from dictknife import loading

parser = argparse.ArgumentParser()
parser.addArgument("--out", default=None)
parser.addArgument("--format", default=None, choices=["json", "yaml"])
args = parser.parse_args()

d = {"name": "foo"}

loading.dumpfile(d, args.out, format=args.format)
```

#### dictknifeにsort_keysオプションを追加した

ただこの過程で１つ問題を見つけてしまった。今までdictknifeでは現在の状態をそのまま出力するということでjson.dumpsの`sort_keys`的なオプションを用意していなかったのだけれど。今回のalchemyjsonschemaの変更で欲しくなってしまった。実際sortして出力ができないとコードを実行する度にfieldの順序が変わるので微妙になってしまう。

そんなわけで、dump,dumps,dumpfileに`sort_keys`というオプションを用意した。ついでにyamlでも同様にsortされるようにした。

```python
# 毎回おんなじ出力になる
d = {"name": "foo", "age": 20}
loading.dumpfile(d, args.out, format=args.format, sort_keys=True)
```

### magicalimportを使うようにした

PYTHONPATHに含まれていないファイルに物理的なファイルパスを指定してロードする仕組みに[magicalimport](https://github.com/magicalimport)というライブラリをよく使っている。これも自作のライブラリなのだけれど。

alchemyjsonschemaは過去に作ったライブラリで、このmagicalimportを使っていなかった。pkg_resourcesを直接importしていた。これを止めた。(そういえば、pkg_resourcesを利用するとひどくimportに時間が掛かるという話しを昔に書いた。詳しくは[`pip install -e` でインストールしたpython製のコマンドの起動が異様に遅かった話](http://pod.hatenablog.com/entry/2017/05/04/130523)参照)


この２つの依存が増えたのとpython2.7はサポートされなくなったかも(たぶん）みたいな変更があった。

### ciの適用範囲を3.5だけから3.4,3.5,3.6,3.6-dev,nightlyに変更した

これはそのまま。

```diff
diff --git a/.travis.yml b/.travis.yml
index 39c5322..e7138f6 100644
--- a/.travis.yml
+++ b/.travis.yml
@@ -1,6 +1,11 @@
 language: python
 sudo: false
-python: 3.5
+python:
+  - "3.4"
+  - "3.5"
+  - "3.6"
+  - "3.6-dev" # 3.6 development branch
+  - "nightly"
 install:
   - pip install -e .[testing]
 script: python setup.py test
```

ただこの対応は微妙な気がしていて。pythonの新しいバージョンが出るたびにサポートするバージョン変える作業を毎回発生してしまうのがなんかもう少し良い方法無いかな〜と思ったりしている。

小さめのリポジトリをたくさん作る系の生態の人は辛いのではという気持ちになっている。

## swagger2.0以外の出力形式の対応

去年に変更してalchemyjsonschemaをコマンドとして利用する場合にはswagger2.0(OpenAPISpec 2.0)で出力するという形だけで良いかなと思ったのだけれど。formatで分岐するようにしたついでにOpenAPI3.0にも対応するかという気になった。

と言ってもschema部分に関してはそんなに大変ではなく。`#/definitions`以下に置かれていた定義を`#/components/schemas`以下に置かれるようにするだけ。以下3つのlayoutを指定できるようにする(formatはファイルフォーマットとかぶるし。他に良い名前を思いつかなかったのでとりあえずlayout)。

```shell
# defaultはopenapi2.0
$ alchemyjsonschema models.py --format=yaml

# jsonschemaはクラスまで指定しないとダメ
$ alchemyjsonschema --layout=jsonschema models.py:User

# openapi3.0
$ alchemyjsonschema --layout=openapi3.0 models.py --format=yaml
```

### 実行結果

例えば以下の様なUser,Groupという定義があった時に。

#### python code

```python
import sqlalchemy as sa
from sqlalchemy.ext.declarative import declared_attr, declarative_base
import sqlalchemy.orm as orm

Base = declarative_base()


class IdMixin:
    @declared_attr
    def id(cls):
        for base in cls.__mro__[1:-1]:
            if getattr(base, '__table__', None) is not None:
                type = sa.ForeignKey(base.id)
                break
        else:
            type = sa.Integer

        return sa.Column(type, primary_key=True)


class Group(IdMixin, Base):
    __tablename__ = "Group"

    name = sa.Column(sa.String(255), default="", nullable=False)


class User(IdMixin, Base):
    __tablename__ = "User"

    name = sa.Column(sa.String(255), default="", nullable=True)
    group_id = sa.Column(sa.Integer, sa.ForeignKey(Group.id), nullable=False)
    group = orm.relationship(Group, uselist=False, backref="users")
```

#### openapi2.0用の出力

```shell
$ alchemyjsonschema models.py --format=yaml
```

```yaml
definitions:
  Group:
    properties:
      id:
        type: integer
      name:
        maxLength: 255
        type: string
      users:
        items:
          $ref: '#/definitions/User'
        type: array
    required:
    - id
    title: Group
    type: object
  User:
    properties:
      group:
        $ref: '#/definitions/Group'
      id:
        type: integer
      name:
        maxLength: 255
        type: string
    required:
    - id
    title: User
    type: object
```

#### openapi 3.0用の出力

```shell
$ alchemyjsonschema models.py --format=yaml --layout=openapi3.0
```

```yaml
components:
  schemas:
    Group:
      properties:
        id:
          type: integer
        name:
          maxLength: 255
          type: string
        users:
          items:
            $ref: '#/components/schemas/User'
          type: array
      required:
      - id
      title: Group
      type: object
    User:
      properties:
        group:
          $ref: '#/components/schemas/Group'
        id:
          type: integer
        name:
          maxLength: 255
          type: string
      required:
      - id
      title: User
      type: object
```

## そういえば

そういえば、ドキュメントテキトウなのどうにかしないと。
