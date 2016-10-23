# golang astをvisitorで殴る

- [example_ast](../20161021/example_ast)

# golang input <-> outputのmodelの変換について

apiのresponse用のpresentation modelと内部のmodelがあったとして、その間のデータの変換がだるい。
用途によってpresentataion modelの数は増える。

```
presentation model <-> model (infra)
```

converterの自動生成と言う方向で考えていたけれど。
もう少しスマートUI的なものに捉えてある構造(ここではmodelを指す?)を色々な形状に変換する問題と考えると色々面白いのかも

- [example_flexible_output](./example_flexible_output)
