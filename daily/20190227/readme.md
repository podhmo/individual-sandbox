## jsonschemaのはまりどころ

- validなschema?

  - indent
  - typo

- required?
- additionalProperties

そして分かりづらいエラーメッセージ


## python jsonschemaのエラー内容をわかりやすくは可能？

- xerrorsのWrap的な仕組みでどうにかできないかな？
- そもそもjsonschema.ValidationErrorでアクセスできる情報は

```console
$ pyinspect inspect jsonschema.exceptions:ValidationError

jsonschema.exceptions:ValidationError <- jsonschema.exceptions:_Error <- builtins:Exception <- builtins:BaseException <- builtins:object

jsonschema.exceptions:_Error <- builtins:Exception <- builtins:BaseException <- builtins:object
    [method, OVERRIDE] __init__(self, message, validator=<unset>, path=(), cause=None, context=(), validator_value=<unset>, instance=<unset>, schema=<unset>, schema_path=(), parent=None)
    [method, OVERRIDE] __repr__(self)
    [method, OVERRIDE] __str__(self)
    [method] __unicode__(self)
    [method] _contents(self)
    [method] _set(self, **kwargs)
    [property] absolute_path
    [property] absolute_schema_path
    [class method] create_from(other)
```

- contextが一番わかり易い？ -> そうでもないかも？
- schemaとschema_path
- validatorとvalidator_value

発生理由

- context
- cause

対象

- instance
- path
- schema
- schema_path
- validator
- validator_path

## ast イジる系の操作

- 何かをparseした結果を利用して何かをを置き換えるときが便利
- 特にその置き換えが文脈を考慮したものである場合に便利
- あるいは置き換えが不完全だと困る場合に

## sentinel jsonschema Policy as Codeどうしよう

- jsonschemaとjsonnetとsentinelとパラメーターストアの悪魔合体を考えてみる
- エラーレポーティングを丁寧にしないと使いづらい
- validation

  - フォーマットのチェック
  - 値同士の関係のチェック
  - 永続化された内部のデータとの整合性のチェック

- Policy as Code

  - Policyは構造をclassと捉えたときのmixin的なもの(たしかに欲しい)
  - 最後にjsonschemaなどにcompileされた表現としてDSLを定義ということで上手く行くか？
  - どのポリシーに接触してしまったのかという文脈を用意できるのがすごい便利

    - jsonschemaなどだと全てのエラーが一気にやってくる

  - 永続化された内部のデータが欲しかった場合にどうする？

    - dynamicRuleみたいなものはたぶん悪手だと思う
    - (テンプレートエンジンとかでruleを動的に生成するのも)
    - このあたりAWSのパラメーターストアなどとの組み合わせでどうなるか..

  - モジュールとしてimportできるのはそれだけで強い
  - 別の方法としては実データからのschemaの生成という方針がある

    - exampleが無料でやってくる
    - ただしマスキングの操作やexampleのshrinkが必要になるかもしれない

