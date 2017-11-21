```
git clone git@github.com:golang/go.git
```

- mapの位置
- goのbuild
- binaryを分ける


## goのbuild

- テストなどスキップしてbuildしたい場合にはmake.bashを使う

macなら(macports)

```
GOROOT_BOOTSTRAP=/opt/local/lib/go bash -x make.bash
```

linuxなら(arch)

```
GOROOT_BOOTSTRAP=/usr/lib/go bash -x make.bash
```

cacheなどをそのまま使いたい場合

```
GOROOT_BOOTSTRAP=$(go env | grep GOROOT | cut -d = -f 2) bash -x make.bash --no-clean
```

cross compile

```
GOROOT_BOOTSTRAP=$(go env | grep GOROOT | cut -d = -f 2) GOOS=linux GOARCH=amd64 bash -x make.bash --no-clean
```

### see also

- https://golang.org/doc/install/source#install
