## reflect-openapi

- typeとfuncの順序を気にしたくない
- 名前の衝突を防ぐ

### 順序

thunkで包むか。。結果を利用した操作が使えなくなるのが不便。

### 名前の衝突

修正するポイント

- components の key
- refのpath

## terraform

- remote stateにdate only moduleを持っていきたい
- save時には、objectを
- load時には、idだけを (outputで出力された）

## JSON path

pathのmatching。これはmineのほうが先に読み込まれる。

```
/pets/{pet_id}
/pets/mine
```

任意個とかの"*"とかないのだっけ？

## openapi

- markdownでちょっとした感じで見れる物が欲しい？
- [../20210206/example_doc/docgen.py](~/vboxshare/venvs/my/individual-sandbox/daily/20210206/example_doc/docgen.py)
- 途中でやめていた。

## reflect-openapi shape

- キレイに表示できるようにしてみる
- 手軽に作成できるようにしてみる

selectorのテストをした後何がしたかったのだっけ？

- typeを上書き (tags)
- sql.Nullstringなど対応 (driver.Valuer)

## terraformっぽい何かの実装

っぽいとはなんだろうか？

- 依存関係を記述
- planとapplyの存在

  - ok create,update,destroyの分岐
  - applyのときだけ実行される
  - 中断時にそこまでだけ保存

- backendにいい感じに保存 (ころころ変わるidを参照)

  - ok 自分で作ってそれを保持するからできる

あたらしくterraform的な何かを作ることに価値はあるんだろうか？

- あんまりない。欲しいのはこれらとの繋がり
- ECSで考えると、taskDefinition と cluster, service, task というような関係

  - service, task はコロコロ変わる
  - deploy時には手元のものが最新になる場合がある
  - 動いているものをコピーしてくる

- 巷に存在しているECSのデプロイツールで良いのでは？ (task definitionまで含めていい感じにやってほしいらしい)

## shape的なものを静的にやる方法を考えてみる？

## ecs

- https://github.com/arminc/terraform-ecs

### terraform doc

- https://github.com/terraform-docs/terraform-docs
- https://github.com/cytopia/docker-terraform-docs
- https://github.com/voyagegroup/terraform-aws-ecs-task-sequential-execution/blob/master/Makefile

## api doc

- https://mermade.github.io/reslate/?javascript#kittens
