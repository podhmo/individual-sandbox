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

