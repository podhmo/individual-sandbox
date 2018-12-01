## go stdlib

使いかたを覚えておきたい

- flag
- exec(go-sh)

## go goa

- https://github.com/goadesign/goa

v1

```
cd $GOPATH/src/goa-adder
goagen bootstrap -d goa-adder/design
```

v2

```
cd $GOPATH/src/calcsvc
goa gen calcsvc/design
```

### main.go の生成

```
goa gen github.com/goadesign/goa/examples/calc/design --debug
```

`goa538526107:` のようなディレクトリを作って、そこでbuildする

```
goa538526107/
├── goa
└── main.go

0 directories, 2 files
```

## go mage

- https://github.com/magefile/mage

### main.go の生成

-keep -debug -compile

```
mage -init
# edit
mage -keep ls
```

## go monkey

- https://github.com/bouk/monkey
