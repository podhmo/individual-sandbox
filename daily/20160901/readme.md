# golang 知らなかったこと

- [Go言語によるCLIツール開発とUNIX哲学について - ゆううきブログ](http://blog.yuuk.io/entry/go-cli-unix)
- [コマンドラインツールを作るときに参考にしている資料 | SOTA](http://deeeet.com/writing/2014/08/27/cli-reference/)
- [Goに入ってはGoに従え](http://ukai-go-talks.appspot.com/2014/gocon.slide)

## time.Durationでcastする必要ない。

```go
// 良くない
t := time.Duration(30) * time.Second
fmt.Printf("%[1]T: %[1]v\n", t)

// 良い
t := 30 * time.Second
fmt.Printf("%[1]T: %[1]v\n", t)
```

## closeのdeferは確認したほうが良いらしい?

```go
// http://ukai-go-talks.appspot.com/2014/gocon.slide#11
func run() (err error) {
    in, err := os.Open(*input)
    if err != nil {
        return err
    }
    defer in.Close()

    out, err := os.Create(*output)
    if err != nil {
        return err
    }
    defer func() {
        if cerr := out.Close(); err == nil {
            err = cerr
        }
    }()
    // some code
}
```

## memo: chan struct{}で同期

http://ukai-go-talks.appspot.com/2014/gocon.slide#35

selectにdefault付けた状態だとすぐにblockせず値返ってくるのだっけ？(後で調べる)

```go
func (s *Stream) IsClosed() bool {
    select {
    case <-s.cc:
        return true
    default:
        return false
    }
}
```

# golang buildした生成物の置き場所

- toolとか他の環境でも使う場合 -> $GOPATH/bin以下
- プロジェクトのアプリなど限定された環境で使う場合 -> `go build -o <./bin>` とか

# golang subcommandを作る方法

## subcommand以前

flag packageで頑張れば良い。

辛いこと

- short optionとlong optionの設定
- subcommandの作成

## subcommandの作成

subcommandの作成。この辺りが人気？

- https://github.com/mitchellh/cli
- https://github.com/spf13/cobra

この辺りも少しだけ気になる

- https://github.com/mkideal/cli
- https://github.com/ukautz/clif

個人的にはcobraが良い気がした。clifは慣れると便利かもしれないけれど。少し学習コストが高めな感じ。

やったことは以下だけ。subというsubcommandを作成してみている。

- cmd
- cmd/sub

### spf13/cobra使ってみる

[https://github.com/spf13/cobra]

- [example_cobra](example_cobra)

### mitchellh/cli使ってみる

[https://github.com/mitchellh/cli]

- [example_mitchellh_cli1](example_mitchellh_cli1)
- [example_mitchellh_cli2](example_mitchellh_cli2)

### mkideal/cli使ってみる

[https://github.com/mkideal/cli]

- [example_mkideal_cli](example_mkideal_cli)

### ukautz/clif使ってみる

[https://github.com/ukautz/clif]

- [example_clif](example_clif)

# golang そろそろweb関係のものも見ていく

- [マイクロサービスのための綺麗なAPI設計 by Go Takagi | Wantedly Engineer Blog](https://www.wantedly.com/companies/wantedly/post_articles/32977)
- [shimastripe/go-api-sokushukai:](https://github.com/shimastripe/go-api-sokushukai)

WAFとかも少しだけ覗いておきたい感じがある。echoとか見るのが良いのかな？

## 環境作ってみる。

```bash
$ go get github.com/shimastripe/go-api-sokushukai
$ cd $GOPATH/src/github.com/shimastripe/go-api-sokushukai
$ go get ./...
$ go build -o bin/server
$ bin/server
```

よく分かんないけれど。これはginとgormらしい。

```bash
$ curl "http://localhost:8080/api/users/1?pretty=1"
```

どういう実装になるかは apigを見たほうが良さそうな感じ。

### apig?

[apig](https://github.com/wantedly/apig) が何かわからなかったけれど。api generatorっぽい。

scaffold用のあれこれらしい。

```bash
$ apig new -u wantedly apig-sample
$ apig gen  # 何か色々生成される
```
