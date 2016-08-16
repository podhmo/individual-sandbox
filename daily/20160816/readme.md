# golang selfish

- [これ](https://gist.github.com/podhmo/3cd3c1cd8bb1392dafc2eedc07f3cf35) の課題的なもの

gistのclientのようなものを作る。とりあえずselfishという感じの名前にする。

# [wip] golangの `go build` で生成される名前を変えることできないのかな？

a.out みたいな雑な名前で生成してgitignoreにマッチするようにしたい。
普通に `-o` で設定できたっぽい。

```bash
$ go build  # ディレクトリ名が生成される実行ファイルの名前になるっぽい
$ go help build
$ go build -o <executable name>
```

# [wip] github apiへのアクセス

とりあえずここにある通りのことを写していく。

- :notebook: access tokenはリポジトリに含めないようにしとこ
- [github - GoDoc](https://godoc.org/github.com/google/go-github/github)

access token は [この辺り](https://github.com/settings/tokens) 。

```go
import "golang.org/x/oauth2"

func main() {
	ts := oauth2.StaticTokenSource(
		&oauth2.Token{AccessToken: "... your access token ..."},
	)
	tc := oauth2.NewClient(oauth2.NoContext, ts)

	client := github.NewClient(tc)

	// list all repositories for the authenticated user
	repos, _, err := client.Repositories.List("", nil)
    fmt.Println(repos)
}
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


