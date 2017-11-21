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

## mapの実装のこと

場所は `runtime/hashmap.go` 。

特にイテレートする部分に関しては以下の関数が関係ある

- mapiterinit
- mapiternext

ちなみに実際の操作では以下の関数から呼ばれる

- reflect_mapiterinit
- reflect_mapiternext
- reflect_mapiterkey

ところでmapの構造には以下のようなstructが使われる

- hmap hashtableの実装
- hiter iteratorの構造

### 型の情報

型の情報は以下の様な型

- maptype これ自体は `runtime/type.go` で定義されている

### rangeが走った時の実装

`compile/internal/gc/range.go` の `walkrange`
これらの開始地点は、`compile/internal/gc/pgen.go` の `compile`
さらに言えば、gc/main.goのMainの中でdcl.goのfunccompile越しに呼ばれてる。
