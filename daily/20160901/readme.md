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

## spf13/cobraつかってみる

[https://github.com/spf13/cobra]

- example_cobra

## mitchellh/cliつかってみる

[https://github.com/mitchellh/cli]

- example_mitchellh_cli1
- example_mitchellh_cli2


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
