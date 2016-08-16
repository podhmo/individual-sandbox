# golang selfish

- [これ](https://gist.github.com/podhmo/3cd3c1cd8bb1392dafc2eedc07f3cf35) の課題的なもの

gistのclientのようなものを作る。とりあえずselfishという感じの名前にする。

# [wip] github apiへのアクセス

とりあえずここにある通りのことを写していく。

- :notebook: access tokenはリポジトリに含めないようにしとこ
- [github - GoDoc](https://godoc.org/github.com/google/go-github/github)

```go
import "github.com/google/go-github/github"


client := github.NewClient(nil)

// list all organizations for user "willnorris"
orgs, _, err := client.Organizations.List("willnorris", nil)
```

# [wip] golang JSONのpretty print

encoding/jsonのところに `Indent()` という関数が存在。

```go
type Road struct {
    Name   string
    Number int
}
roads := []Road{
    {"Diamond Fork", 29},
    {"Sheep Creek", 51},
}

b, err := json.Marshal(roads)
if err != nil {
    log.Fatal(err)
}

var out bytes.Buffer
json.Indent(&out, b, "=", "\t")
```

- [json を pretty print するのに echo '{"apple": "red", "lemon": "yellow"}' | python -m json.tool は冗長じゃないですか？なので go でコマンド用意しました - Qiita](http://qiita.com/ikawaha/items/8a01c5739401e26e8794)
- [json - The Go Programming Language](https://golang.org/pkg/encoding/json/#Indent)


