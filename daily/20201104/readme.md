## swagger marshmallow codegen リファクタリングを考えてみる

SchemaWriterはなぜ此処まで複雑なのか？
基本的にはすべての事柄がSchemaWriterで行われている。

- SchemaWriter

  - write_schema

    - write_schema
    - write_primitive_schema
    - write_field_one
    - write_field_many

  - write_primitive_schema

    - write_field_one
    - write_field_many

  - write_field_one
  - write_field_many

    - writ_field_one

どういう条件を考慮している？

- primitive schemaはシンプル。runtimeに依存している
- write_field_manyは頑張ればwrite_field_oneに吸収できる?

  - 苦肉の策でwrapという関数を定義しているのがヤバイ

- write_field_oneはnestedを見つけたところからの分岐が長い

  - 現実的には、refをどうにか最終ゴールにしようとしているのではないか？

- write_schemaは本当にヤバイ

  - 到達していたら無視
  - refを持っていたら
  
    - resolve_ref_definition()で、definitionを取り出そうとしている

      - arrayがあったときには、名前だけをそこからさらに取り出す
      - refが見つからなくなった時点で現在保持していたnameをtitleizeして返す

    - itemsで分岐
    
      - refを持っていたら最終地点を取得
      - schemaを持っていたら -> write_schema (itemsを利用して)
      - schemaを持っていなかったら -> write_primitive_schema (itemsを利用して)
      - base_classesを生成したschemaのものとして保持

  - allOfを持っていたら
  
    - 全部をdeepmerge
    - それっぽいクラスを作る

  - additionalPropertiesを保持していて、それがdictなら -> AdditionalSchema
  - propertiesがなく、itemsもない -> primitiveSchema
  - itemsを持っていたら -> many=trueでなんか頑張る

読むと意外とそんなにひどくもないな。。

思うこと

- refを取り出す作業とloopを分けては？

### prestringの機能をもう少し上手に使えない？

- LazyFormatだけで対応しているので辛いような気がしている。
- LazyArgumentsAndKeywordsが使えるのでは？
- あと、field名への代入をDict部分も再帰で作れるような気がしている。

もう少し整理してみる。

```py
# primitive
fields.String()
# primitive with required
fields.String(required=True)

# many
fields.List(fields.String())
# many with required
fields.List(fields.String(), required=True)
# many with nested
fields.List(fields.Nested(lambda: X()), required=True)

# nested
fields.Nested(lambda: X())
# nested with required
fields.Nested(lambda: X(), required=True)

# dict
fields.Dict(keys=fields.String(), values=fields.String())
# dict with required
fields.Dict(keys=fields.String(), values=fields.String(), required=True)
# dict with nested
fields.Dict(keys=fields.String(), values=fields.Nested(lambda x: Nested()))
```

分解してみる

```
{field_name} = {field_caller}({arguments})
```

ここで、argumentsにはいろいろ入る。これだけでは？もっと言えばargumentsは以下の様に再帰する

```
<name> '(' <arguments> ')'
arguments := '(' ( <value> <options> )')'

value := ε | <primitive> | <list> | <nested> | <dict>
primitive := <name>()
list: 'fields.List(' <value> ')'
nested: 'fields.Nested(lambda: ' <value> '())'
dict: 'fields.Dict(keys=fields.String(), values=' <value> ')'
options := ε | ',' <tk> <options>
```

## python

yesterday, I repaid swagger-marshmallow-codegen.
And running tiny rpc examples, and web api serialization/deserialization examples.


SchemaWriterはなぜ此処まで複雑なのか？
基本的にはすべての事柄がSchemaWriterで行われている。

- SchemaWriter

  - write_schema

    - write_schema
    - write_primitive_schema
    - write_field_one
    - write_field_many

  - write_primitive_schema

    - write_field_one
    - write_field_many

  - write_field_one
  - write_field_many

    - writ_field_one

どういう条件を考慮している？

- primitive schemaはシンプル。runtimeに依存している
- write_field_manyは頑張ればwrite_field_oneに吸収できる?

  - 苦肉の策でwrapという関数を定義しているのがヤバイ

- write_field_oneはnestedを見つけたところからの分岐が長い

  - 現実的には、refをどうにか最終ゴールにしようとしているのではないか？

- write_schemaは本当にヤバイ

  - 到達していたら無視
  - refを持っていたら
  
    - resolve_ref_definition()で、definitionを取り出そうとしている

      - arrayがあったときには、名前だけをそこからさらに取り出す
      - refが見つからなくなった時点で現在保持していたnameをtitleizeして返す

    - itemsで分岐
    
      - refを持っていたら最終地点を取得
      - schemaを持っていたら -> write_schema (itemsを利用して)
      - schemaを持っていなかったら -> write_primitive_schema (itemsを利用して)
      - base_classesを生成したschemaのものとして保持

  - allOfを持っていたら
  
    - 全部をdeepmerge
    - それっぽいクラスを作る

  - additionalPropertiesを保持していて、それがdictなら -> AdditionalSchema
  - propertiesがなく、itemsもない -> primitiveSchema
  - itemsを持っていたら -> many=trueでなんか頑張る

読むと意外とそんなにひどくもないな。。

思うこと

- refを取り出す作業とloopを分けては？

### prestringの機能をもう少し上手に使えない？

- LazyFormatだけで対応しているので辛いような気がしている。
- LazyArgumentsAndKeywordsが使えるのでは？
- あと、field名への代入をDict部分も再帰で作れるような気がしている。

もう少し整理してみる。

```py
# primitive
fields.String()
# primitive with required
fields.String(required=True)

# many
fields.List(fields.String())
# many with required
fields.List(fields.String(), required=True)
# many with nested
fields.List(fields.Nested(lambda: X()), required=True)

# nested
fields.Nested(lambda: X())
# nested with required
fields.Nested(lambda: X(), required=True)

# dict
fields.Dict(keys=fields.String(), values=fields.String())
# dict with required
fields.Dict(keys=fields.String(), values=fields.String(), required=True)
# dict with nested
fields.Dict(keys=fields.String(), values=fields.Nested(lambda x: Nested()))
```

分解してみる

```
{field_name} = {field_caller}({arguments})
```

ここで、argumentsにはいろいろ入る。これだけでは？もっと言えばargumentsは以下の様に再帰する

```
<name> '(' <arguments> ')'
arguments := '(' ( <value> <options> )')'

value := ε | <primitive> | <list> | <nested> | <dict>
primitive := <name>()
list: 'fields.List(' <value> ')'
nested: 'fields.Nested(lambda: ' <value> '())'
dict: 'fields.Dict(keys=fields.String(), values=' <value> ')'
options := ε | ',' <tk> <options>
```

