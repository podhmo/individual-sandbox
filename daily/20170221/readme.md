# golang 謎な構造のリストをiterate

http://olivere.github.io/elastic/ から

こういう感じでつかうやつ。

```go
  var ttyp Tweet
  for _, item := range searchResult.Each(reflect.TypeOf(ttyp)) {
    if t, ok := item.(Tweet); ok {
      fmt.Printf("Tweet by %s: %s\n", t.User, t.Message)
    }
  }
```

reflect.TypeOfの使い方が難しい。
