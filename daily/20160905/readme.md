# golang go1.7に上げてからgocodeが動かなくなった。

- [After An Update, "PANIC" Is The Only Gocode Suggestion · Issue #433 · joefitzgerald/go-plus](https://github.com/joefitzgerald/go-plus/issues/433)

以下の様にしたら治った。

```bash
$ gocode close
$ go get -u github.com/nsf/gocode
```

## もしかしてgolintの方のpanicも同様？

- [panic: inconsistent import on go 1.7 · Issue #231 · golang/lint](https://github.com/golang/lint/issues/231)

```bash
$ rm -r $GOPATH/pkg/*
$ go get -u github.com/golang/lint/golint
```

# golang emacs

- 標準ライブラリにも移動したい
- prefixが邪魔

直した。ついでに C-c : で動くようにした。

# wip golang 外部コマンドの実行

`os/execを使えば良い？`

[example_exec](example_exec)

- output
- start,stop
- pipe
- context-based

# golang 読む

- cybozu-go/cmd
- wantedly/apig

