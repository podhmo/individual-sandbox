## schemalint

後々の話

- docker image?
- daemon process?
- schema object cache?

### 過去に作ったものの整理

[../20190816/example_linter/readme.md](../20190816/example_linter/readme.md)

- 15parse.py がyamlのloadまで
- 17parse.py がvalidation schema

### とりあえず繋げる

1. cli.pyにベタに直書き
1. パッケージを分ける
1. jsonschemaのvalidationを追加する

validationを追加する部分でdetectorのinterfaceを書き換える必要がありそう。
全体をiteratorに直したい。
