jsonschemaによるsyntax checkがしたい。

```
pip install -r requirements.txt
```

ちょっと先は長い

- jsonschemaでエラーになった部分の行番号を適切に取れない可能性がある。
- (まじめにやると、改行を含んだり含まなかったりする元の入力文字列に対してJSONPath的なもので行数を取得する必要がある)

