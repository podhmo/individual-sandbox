# go かんたんな用例のpackageの分割

細かすぎると使いづらい一方でnamespaceはほしい。gistは諦めて以下の様な構造にするのが無難

- main.go
- foo/xxx.go

main.goの中で `.foo` でimport

# go 依存したタスクの協調的な操作(昨日の続き)

- [昨日の作業](../20161003/example_objectid/) の続き

