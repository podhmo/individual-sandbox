# go connect-go

- https://github.com/bufbuild/connect-go
- https://connect.build/docs/go/getting-started/
- https://docs.buf.build/introduction

## やること

- connect-go用の諸々のツールをインストール
- getting started
- buf.yamlとbuf.gen.yamlの設定の意味の理解 
- 依存の持ち方の理解


## getting started 触る

- https://connect.build/docs/go/getting-started/


### setup go modules

```
mkdir connect-go-example
cd connect-go-example
go mod init example
go install github.com/bufbuild/buf/cmd/buf@latest
go install github.com/fullstorydev/grpcurl/cmd/grpcurl@latest
go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
go install github.com/bufbuild/connect-go/cmd/protoc-gen-connect-go@latest
```

### initialize (scaffold)

```
mkdir -p greet/v1
touch greet/v1/greet.proto
```

### first running the buf command

```console
# generate buf.yaml
$ buf mod init
$ touch buf.gen.yaml
```

buf.gen.yamlを書く

```console
$ buf lint
$ buf ls-files
greet/v1/greet.proto
$ buf generate
```

### writing main.go

```console
$ mkdir -p cmd/server
$ touch cmd/server/main.go
```

main.goを書く

```console
$ go get golang.org/x/net/http2
$ go get github.com/bufbuild/connect-go
$ go run ./cmd/server/main.go
```

### requests

```console
$ echo '{"name": "Jane"}' | http --json POST :8080/greet.v1.GreetService/Greet
HTTP/1.1 200 OK
Accept-Encoding: gzip
Content-Encoding: gzip
Content-Length: 51
Content-Type: application/json
Date: Mon, 17 Oct 2022 20:08:26 GMT
Greet-Version: V1

{
    "greeting": "Hello, Jane!"
}
```

or grpc

```console
$ grpcurl -protoset <(buf build -o -) -plaintext -d '{"name": "Jane"}' localhost:8080 greet.v1.GreetService/Greet
{
  "greeting": "Hello, Jane!"
}
```