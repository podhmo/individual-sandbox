# configの読み込みに便利なものを探していた

元々はこの記事が発端

- [GoでCUEのバリデーション機能を利用しつつ、.cue/.json/.yaml形式の設定ファイルを読み込む](https://future-architect.github.io/articles/20191030/)

.cueを無視してとりあえずvalidation部分を他のものでもpythonでというような方針でやろうとした。
ついでにどのような形式かjsonschemaで出力できれば良いなともおもった。

あと以下の条件を追加している

- ネストした表現に対応しているか
- 自己参照的にネストした表現に対応しているか
- 設定の形状を何らかの方法で可視化する術を用意しているか (e.g. jsonschemaとして出力)

## 候補

- [pydantic](https://github.com/samuelcolvin/pydantic)
- [typesystem](https://github.com/encode/typesystem)
- [marshmallow](https://github.com/marshmallow-code/marshmallow) (+ [marshmallow-jsonsschema](https://github.com/fuhrysteve/marshmallow-jsonschema)

### pydantic

schema単体の書き心地はpydanticが一番楽だった。あとmypyフレンドリー。
型を指定すればrequired=Trueで生成されるので。
ただ自分自身に再帰的に指定するような型の指定では`update_forward_refs()`を呼ばなければいけないのが面倒だった。

あと参照関係のないschemaの場合にはdataclassesを使うのがとてもしっくり来た。なるべくなら特殊な型を継承したくはない。一方でこのdataclassesもどきを利用してjsonschemaを出力する方法が分からなかった。

試した場所は

- [00pydantic](00pydantic)
- [03dataclasses](03dataclasses)

### typesystem

これはyaml,jsonのvalidationのタイミングでエラー箇所とロード箇所の対応を取る機能があるので、エラーメッセージ的には上手く使えれば最高の体験になりそうとはおもった(tokenized_errors)。しかし自己参照する再帰などが行えないなど機能的に不足している部分が多そうだった。そしてjsonschema用のdefinitions用の管理が微妙

- https://www.encode.io/typesystem/tokenized_errors/

試した場所は

- [01typesystem](01typesystem)

### marshmallow + jsonschema

defaultがunrequiredなのがちょっとrequiredオプションを忘れると辛いかもしれない。あと複数のモジュールをインポートする必要がある。読みやすくはあるのだけれど、手軽な描き心地は得られないかもしれない。あとおそらくmypyの恩恵はなかなか得られなそう。

(実は一部生成不可能な形状もあるのだけれど(e.g. トップレベルのarray))、一番生成されたjsonschemaが自然な表現だったように見える。`additionalProperties:false`も付くのでそこそこstrictな記述になる。

- [04marshmallow](04marshmallow)
