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


### rangeのとき


- mapiternextが繰り返し呼ばれる
- iterator(hiter)を作ったタイミングで雑に開始時のbucketを選択する
- あとはそのbucketからじゅんじゅんに読んでく。

randomになっているのは

- map作成時にhash0を計算してる
- iterator作成時にも開始地点のbucketを探すためにfaastrandしてる
- (hashmap_fastの方もわすれずに)

### debug print

以下のような

```
$ go run --gcflags="-m" <>
```

どういうふらぐがあるかはgo/src/cmd/compile/internal/gc/main.go

### walk

int64の時の最適化を省略した時ビルド時に以下の様なエラーが出る。

```
        STALE cmd/asm: stale dependency: runtime/internal/atomic
        STALE cmd/cgo: stale dependency: runtime/internal/atomic
        STALE cmd/compile: stale dependency: runtime/internal/atomic
        STALE cmd/link: stale dependency: runtime/internal/atomic
        STALE runtime/internal/sys: build ID mismatch
```

これは原因は`--no-clean`をつけたままbuildしているせい。


### hash0,rをすべて固定値にしてもだめかも

− そもそもkeyの値はpointerを元に計算されていそう
- あと大きくしたりのヒューリスティックにも乱数が使われている（無効にできなくもないけれど）


### 結局頑張ってlinked listで順番保持してる

hlinkという構造を別途作った。reflect.goの方も書き換えないといけないので結構大変。

### int64等用の個別の関数に上手くヒットできていない気がする？

何か使いかたを誤っている。

通常は？

```
hash := t.key.alg.hash(key, uintptr(h.hash0))
b := (*bmap)(add(h.buckets, (hash&m)*uintptr(t.bucketsize)))
for i := uintptr(0); i < bucketCnt; i++ {
    k := add(unsafe.Pointer(b), dataOffset+i*uintptr(t.keysize))
    if alg.equal(key, k) {
    	v := add(unsafe.Pointer(b), dataOffset+bucketCnt*uintptr(t.keysize)+i*uintptr(t.valuesize))
    }
}
```

ようはbucketからの差分を取り出しているということはkeyを保存しようとしてもだめ。
どうやってuniqueなidにしよう。。

たとえば、uint32の場合はkeyの生成は

```
// t *maptype key uint64
hash := t.key.alg.hash(noescape(unsafe.Pointer(&key)), uintptr(h.hash0))
m := bucketMask(h.B)
b = (*bmap)(add(h.buckets, (hash&m)*uintptr(t.bucketsize)))
for i, k := uintptr(0), b.keys(); i < bucketCnt; i, k = i+1, add(k, 7) {
	add(unsafe.Pointer(b), dataOffset+bucketCnt*8+i*uintptr(t.valuesize)
}

// t *maptype key uint32
hash := t.key.alg.hash(noescape(unsafe.Pointer(&key)), uintptr(h.hash0))
bucket := hash & bucketMask(h.B)
b = (*bmap)(add(h.buckets, (bucket)*uintptr(t.bucketsize)))
for i, k := uintptr(0), b.keys(); i < bucketCnt; i, k = i+1, add(k, 4) {
    return add(unsafe.Pointer(b), dataOffset+bucketCnt*4+i*uintptr(t.valuesize))
}
```

- どちらでもkeyはunsafe
- access2は存在確認もする

assignのときは？

```
hash := t.key.alg.hash(noescape(unsafe.Pointer(&key)), uintptr(h.hash0))
bucket := hash & bucketMask(h.B)
b := (*bmap)(unsafe.Pointer(uintptr(h.buckets) + bucket*uintptr(t.bucketsize)))
var insertk unsafe.Pointer
insertk = add(unsafe.Pointer(insertb), dataOffset+inserti*4)
```
