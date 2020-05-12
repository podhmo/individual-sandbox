## やりたいこと

`dst/`以下のファイルを生成したい。

- タスクの指定なしの場合には、全ての古い`dst/`以下のファイルが更新されてほしい
- 特定のファイルだけを選択した場合には、特定のファイルに対する依存だけを見て更新してほしい。
- 不要なビルドはスキップされてほしい

```console
$ tree
.
├── Makefile
├── README.md
├── _tools
│   ├── gen.py
│   ├── x-go.j2
│   ├── x-yaml.j2
│   └── y-go.j2
├── dst
│   ├── x.go
│   ├── x.yaml
│   └── y.go
├── pre
│   └── dst
│       ├── x.go.py
│       ├── x.yaml.py
│       └── y.go.py
├── root.yaml
└── x.csv

4 directories, 14 files
```

## 問題点

複雑。
