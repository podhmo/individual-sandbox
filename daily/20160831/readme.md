# wip golang tool作るのに見ていきたい感じのものたち

https://github.com/cybozu-go/

:notebook: あとでよさそうなものpickupしておく。

# wip emacs golang

# golang goの諸々

```
$ go tool dist env
# GOROOTの位置に移動
$ go tool dist env | grep GOROOT | cut -d = -f 2 | xargs cd
```

## なぞ

godocインストール時にgorootに入ってしまう？

```
$ go get golang.org/x/tools/cmd/godoc
go install golang.org/x/tools/cmd/godoc: open /opt/local/lib/go/bin/godoc: permission denied
```

```
$ sudo -E go get golang.org/x/tools/cmd/godoc
```

## なぞ 解決したい

[この辺?](https://github.com/golang/go/wiki/InstallTroubleshooting#why-does-go-get-work-for-some-packages-and-report-permission-denied-in-goroot-for-some-others-with-gopath-set-properly)

コード読もう。 [この辺り](https://github.com/golang/go/blob/release-branch.go1.6/src/cmd/go/pkg.go#L807)

そして 1.7では [個別のmappingは消えている](https://github.com/golang/go/blob/release-branch.go1.7/src/cmd/go/pkg.go#L693)。

```
GOBIN=$GOPATH/bin go install golang.org/x/tools/cmd/godoc
```

とりあえずこれで誤魔化せそうだし。1.7からなら無視して大丈夫そう。
