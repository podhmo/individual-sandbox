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
