## go channelでつながられている処理について依存するproviderの部分だけを実行したい

元々の依存

```
A -> X
  -> Y
B -> Y
  -> Z
C -> X
  -> Z
```

たとえば、Xだけを選択したい。(このとき使われないBは実行されない)

```
A -> X
C -> X
```

ただしそれぞれの出力は異なる。fun outではなくchannelベースで渡される値が異なる。

```
providerA - modelatorI -> AForX - consumerX
                       -> AForY - consumerY
providerB - modelatorJ -> BForY - consumerY
                       -> BForZ - consumerZ
providerC - modelatorK -> CForX - consumerX
                       -> CForZ - consumerZ
```

