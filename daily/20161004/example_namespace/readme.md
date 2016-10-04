# namespaceの調査

## 問題意識

ファイルを分割した時にどのパッケージからのimportだったのかが分かるようにnamespaceはほしい。
安易に分割して全部main packageなどの例は逆に見づらい。

一方でまじめにnamespaceを使うとフラットな構造ではなくなるのでgistにあげ辛い。

## 正しい感じの方法

このような構造にした場合には `.foo` をimportすることでごまかせる

```sh
$ tree
.
├── foo
│   └── foo.go
├── main.go
└── readme.md

1 directory, 3 files
```

