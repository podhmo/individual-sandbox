# jwt python go jwtあたりのサンプルでgoで生成されたshared libraryをpythonから使う方法を

3パターンくらい

- JSONでやり取り
- `map[string]string`をやり取り
- builderを公開

# git grepで対象のファイルを絞る

.goのみを対象にする

```
git grep <pattern> -- '*.go'
```

.jsを省く

```
git grep <pattern> -- '!*.html'
```
