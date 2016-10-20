- models 対象のmodel

## setup

```
make setup
```

## 00 iteration

```
go run 00*/main.go $GOPATH/src/github.com/`whoami`/models
```

## misc

gotypeをいじってみる

```
$ go get -v golang.org/x/tools/cmd/gotype
$ go get -v github.com/davecgh/go-spew/spew
gotype --ast $GOPATH/src/github.com/`whoami`/models | sed "s@$GOPATH/src/github.com/`whoami`/models@.@g" > misc/models.ast.dat
go run 01gotypeexample/main.go $GOPATH/src/github.com/`whoami`/models | sed "s@$GOPATH/src/github.com/`whoami`/models@.@g" > misc/models.node.dat
go run 02*/main.go $GOPATH/src/github.com/`whoami`/models | sed "s@$GOPATH/src/github.com/`whoami`/models@.@g" > misc/models.02.txt
```

