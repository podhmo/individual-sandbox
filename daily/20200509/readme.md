## grpc go

- https://qiita.com/marnie_ms4/items/4582a1a0db363fe246f3
- https://grpc.io/docs/quickstart/go/

```
go get -u -v google.golang.org/grpc
go get -u -v github.com/golang/protobuf/protoc-gen-go
go get -u -v github.com/pseudomuto/protoc-gen-doc/cmd/protoc-gen-doc

```

protocもいる

```
PROTOC_ZIP=protoc-3.11.4-osx-x86_64.zip
curl -OL https://github.com/protocolbuffers/protobuf/releases/download/v3.11.4/$PROTOC_ZIP
sudo unzip -o $PROTOC_ZIP -d $HOME/.local
rm -f $PROTOC_ZIP
```

## go wafを試す

このあたりを試すか

- gin-gonic/gin
- echo
- go-chi

### 追記 gin

- gin意外といろいろやってくれる
- どれを使えば良いか迷って地味に面倒

### 追記 go-chi

- go-chiがやっぱり一番 net/http に近い
- renderは必要かというとそうでもないけど

### 追記 echo

- validatorも自前で登録するのか
- contextがやっぱり邪魔な気がする。

## egoist

struct

- new typeとか設定したい
- importを気にしたい
- walker部分は汎化できるのでは？
