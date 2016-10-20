- models 対象のmodel

## setup

```
make setup
```

## misc

gotypeをいじってみる

```
$ go get -v golang.org/x/tools/cmd/gotype
$ gotype --ast $GOPATH/src/github.com/`whoami`/models | sed "s@$GOPATH/src/github.com/`whoami`/models@.@g" > misc/models.ast.dat
```
