# 妖しいもの

gistにuploadしたものをpackageとして利用できるみたいな感じ？(古いのでだめかも)

- [ImJasonH/go-gist: Go import redirector for GitHub Gists](https://github.com/ImJasonH/go-gist)

# golang コマンド的なものの作り方

以下の２つの方法がありそう。(pecoなどは後者)

- すごくすごく単純なもの
- それなりに階層が複雑なもの

## すごくすごく単純なもの

repository名と同一のファイルを１個作る。readme.mdもあっても良い。

例えば、 `https://github.com/<username>/foo` に `foo.go` を置いたrepositoryを作る。
すると `go get github.com/<username>/foo` で `$GOPATH/bin` に入る。

## それなりに階層が複雑なもの

だいたい以下の様な構成になっていることが多い。今回はfooというpackage(コマンド名でもある)を作る場合の例。

```
foo
├── cmd
│   └── foo
│       └── main.go
└── something.go
```

これで `go get github/<username>/foo/cmd/foo` でインストールできるようになる。
またpackageの書き方は概ね以下の様な感じ。

トップレベルのfoo(e.g. something.go)内

```go
package foo

import (...)
```

コマンド部分の階層(e.g. cmd/foo/main.go)

```go
package main

import (
    github.com/<username>/foo
)

// foo.XXX みたいな形で使う。
```
