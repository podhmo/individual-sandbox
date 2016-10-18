# go `panic: reflect.Value.SetFloat using unaddressable value`

```go
var x float64 = 3.4
v := reflect.ValueOf(x)
v.SetFloat(7.1) // Error: will panic.
```

[example_reflection](./example_reflection)

# git 異なるbranch間でdiffを取る

```
$ git diff <branch1> <branch2> [file]
$ git diff <branch1>:[file] <branch2>:[file]
```

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

# goole auth

- [Using OAuth 2.0 to Access Google APIs  |  Google Identity Platform  |  Google Developers](https://developers.google.com/identity/protocols/OAuth2)
- [Using OAuth 2.0 for Web Server Applications  |  Google Identity Platform  |  Google Developers](
https://developers.google.com/identity/protocols/OAuth2WebServer)
- [The correct use of the state parameter in OAuth 2 | Thread Safe](http://www.thread-safe.com/2014/05/the-correct-use-of-state-parameter-in.html)
