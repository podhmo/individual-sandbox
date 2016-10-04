# go かんたんな用例のpackageの分割

細かすぎると使いづらい一方でnamespaceはほしい。gistは諦めて以下の様な構造にするのが無難

- main.go
- foo/xxx.go

main.goの中で `.foo` でimport
