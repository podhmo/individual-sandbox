# golang go-structjson

- package名のfullnameも取るようにした
- 再帰的に必要なpackageのstruct取るようにした(e.g. bson.ObjectId用にbsonパッケージのものも取ってくる)

# python golang 型の変換のつづき

[これ](../20161024/example_conversion)の続き

# python dictをloopする時の注意

以下みたいなコード結果不定になるので止めたほうが良い。

```
for k in d.keys():
    d[k.lower()] = d.pop(k)
```

しかし小さなコード片で実行してみると結果が変わらない。ふしぎ。

# python ちょっと文字列の編集距離が欲しい

レーベシュタイン距離よりも良いものがあるらしい。

- [高速な編集距離の計算方法 - Qiita](http://qiita.com/aflc/items/f4299700471cc11f1d1c)
- [editdistance 0.3.1 : Python Package Index](https://pypi.python.org/pypi/editdistance)

```
>>> import editdistance
>>> editdistance.eval("aaab","baaa")
2
>>> editdistance.eval("aaab","aaaa")
1
```

# python max by 的な処理がしたい時

```
>>> d = {1: 2, 2: -10}
>>> max(d.items(), key=lambda x: x[1])
(1, 2)
>>> max(d.items(), key=lambda x: abs(x[1]))
(2, -10)
```
