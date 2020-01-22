## monogusa やりたいことをまとめたい

- やりたいことのメモを書く
- 次にそれの実装のための試行錯誤を追加していく

## go `xxx is a program, not an importable package`

- https://visdap.blogspot.com/2019/03/how-do-go-modules-work-with-installable.html
- https://christina04.hatenablog.com/entry/go-cmd-tools-versioning

## go get private repository

- https://songmu.jp/riji/entry/2019-07-29-go-private-modules.html

### 410 Gone

hmm


```console
$ go get -v github.com/podhmo/dot-files/go-tools
go: finding github.com/podhmo/dot-files/go-tools latest
go: finding github.com/podhmo/dot-files latest
go: downloading github.com/podhmo/dot-files/go-tools v0.0.0-20200121230420-12f95183ac28
go: downloading github.com/podhmo/dot-files v0.0.0-20200121230420-12f95183ac28
verifying github.com/podhmo/dot-files/go-tools@v0.0.0-20200121230420-12f95183ac28: github.com/podhmo/dot-files/go-tools@v0.0.0-20200121230420-12f95183ac28: reading https://sum.golang.org/lookup/github.com/podhmo/dot-files/go-tools@v0.0.0-20200121230420-12f95183ac28: 410 Gone
```

- https://medium.com/mabar/today-i-learned-fix-go-get-private-repository-return-error-reading-sum-golang-org-lookup-93058a058dd8

```console
$ go env -w GOPRIVATE=gitlab.com/<repo>
```

## env dot-files

いい加減dot-filesのリポジトリを作ろう。。
必要なものは何があるだろう？

- go-tools
- python-tools
- .screenrc
- .bash.profile

## python jedi

そういえばjediのapiを揃えていない

## やりたいこと
