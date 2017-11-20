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

### see also

- https://golang.org/doc/install/source#install
