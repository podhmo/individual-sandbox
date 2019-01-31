## python jsonpatch

- http://jsonpatch.com/

operation

- add
- remove
- replace
- move
- copy
- test

:warning: "/" は `{"": <value>}` の""部分を表す。

### unpatch

jsonpatchのsubsetになるのでは？

- add, remove, replaceだけ使う

:though-balloon: たぶんdiff

### 実装

- https://github.com/cujojs/jiff
- https://github.com/stefankoegl/python-json-patch

## go goreleaser.yml

release用のアレコレしてくれるやつ。

```yaml
builds:
  - main: cmd/<cmd name>/main.go
    binary: <cmd name>
    goos:
      - windows
      - darwin
      - linux
    goarch:
      - amd64
release:
  github:
    owner: podhmo
    name: <cmd name>
```

## go circleci

- [Language Guide: Go - CircleCI](https://circleci.com/docs/2.0/language-go/ "Language Guide: Go - CircleCI")
- [Enabling module support for Go v1.11 in CircleCI - CircleCI](https://circleci.com/blog/go-v1.11-modules-and-circleci/ "Enabling module support for Go v1.11 in CircleCI - CircleCI")

とりあえず `.circleci/config.yml`に書けば良いっぽい。

- version
- jobs
- executors
- workflows

## go golangci-lint

```console
# hmm
go get: warning: modules disabled by GO111MODULE=auto in GOPATH/src;
        ignoring go.mod;
        see 'go help modules'
github.com/golangci/golangci-lint (download)
^
$ GOMODULE111=on go get -u -v github.com/golangci/golangci-lint/cmd/golangci-lint

# command not found?
$ golangci-lint run
$ golangci-lint run --enable-all -v
```

? GOMODULE111=on した状態でのcommandってどこにあるんだろう？

### 使われているlinterの一覧ってどうすれば良いんだろう？


```console
$ golangci-lint linters
$ golangci-lint linters -p bugs
```

[output](./linters.output)

preset毎の設定ってどこで分かるんだろう？

```
  -p, --presets strings             Enable presets (bugs|unused|format|style|complexity|performance) of linters. Run 'golangci-lint linters' to see them. This option implies option --disable-all
      --fast                        Run only fast linters from enabled linters set (first run wo
```

### 設定ってどうやるんだろう？

.golanggi.tomlとか書けば良いっぽい？

https://github.com/golangci/golangci-lint#config-file


## go ci

この辺参考にすれば良いのでは？

- https://github.com/po3rin/gotree

必要なこと

- go ci linter
- circleci

## go example test

そういえば手に付いていない
