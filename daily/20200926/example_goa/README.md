goaの出力するコードってどんなだったっけ？ということを思い出してみる。

## install

```console
$ go mod init m
$ GO111MODULE=on go get -u goa.design/goa/v3/...@v3
$ mkdir design
```

## write design

write design https://github.com/goadesign/goa#1-design

## gen

```console
$ goa gen m/design

```

