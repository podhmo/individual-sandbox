# goの魔改造が間に合わなかったのでそのついでに理解したmapの実装について

この記事は[wacul アドベントカレンダー](https://qiita.com/advent-calendar/2017/wacul)の２日目の記事です。

## はじめに

### goでコード生成やると思います。go-swaggerを使っています

goのコードを書いているときにコード生成に手を染めたいということあると思います。生成されたコードを各自のリポジ# goの魔改造が間に合わなかったのでそのついでに理解したmapの実装について

この記事は[wacul アドベントカレンダー](https://qiita.com/advent-calendar/2017/wacul)の２日目の記事です。

## はじめに

### goでコード生成やると思います。go-swaggerを使っています

goのコードを書いているときにコード生成に手を染めたいということあると思います。生成されたコードを各自のリポジトリに格納するかしないかは判断に分かれるところですが。waculではリポジトリに生成されたコードも格納しています。

waculでは[go-swagger](https://github.com/go-swagger/go-swagger)でコード生成をしています。go製のツールです。go-swaggerについては、APIの仕様ドキュメントをJSON/YAMLで記述しておくと良い感じに実行可能なドキュメント環境になったり複数の言語間でのサーバー/クライアント部分のコードを生成してくれたりする位の雑な理解で良いです。何かファイルを渡すとコードを生成してくれるというところが重要です。

### go-swaggerの出力は順序が毎回バラバラ

さてそのようなgo-swaggerによるコード生成ですが、現在のバージョンでは同じドキュメントを元にコード生成を行おうとしても生成結果に差分が出てきてしまいます。論理的には同じコードなのですが、関数や構造体などの位置が微妙に変わってきてしまいます。例えば、あるパッケージAにf,g,hという順で出力される場合とg,f,hという順で出力する場合などにそれぞれの出力位置が前後したり変わってしまったりして不要な差分が出てしまったりします。

あるときは

```go
func f() {
...
}
func g() {
...
}
func h() {
...
}
```

またあるときは

```go
func g() {
...
}
func f() {
...
}
func h() {
...
}
```


### goのmapに順序なんてあるわけない

これは内部的にはgoのmapを直接利用しているせいです。goのmapをイテレートしたときの順序が一定ではないということが原因です。例えば以下の様なmapをイテレートするようなコードの実行結果は一定にはなりません(あとで詳しく説明しますがgoがあえてそうしています)。

```go
package main

import "fmt"

func main() {
	m := map[string]int{
		"f": 1,
		"g": 2,
		"h": 3,
	}
	for k, v := range m {
		fmt.Printf("%s %d ", k, v)
	}
	fmt.Println()
}
```

ためしに100回位実行してみた結果が以下ですばらつきがありますね。

```console
$ for i in `seq 100`; do go run main.go >> output.txt; done
$ cat output.txt | sort | uniq -c
     76 f 1 g 2 h 3 
     10 g 2 h 3 f 1 
     14 h 3 f 1 g 2 
```

### 魔改造したい

ここで魔改造です。

> goのmapをforでループした結果が順不同になるのがだめなら、順序を保持したmapを利用するように魔改造してしまえば出力が一定になるのでは？という素朴な思いつきを元に魔改造をしてみることにしてみました。

当初は、以上のような導入文で始める記事を書こうというつもりで、魔改造したgoを作ろうと思っていたのですが、間に合いませんでした。間に合いませんでしたたのでその途中でわかったことなどをまとめようと思います。後半戦に続くみたいな感じになるかもしれません。

1日(24時間)くらいあれば無理矢理にでも実装できるかな−と思ったのですが。全然足りなかった感じでした。

## 魔改造のための第一歩goのコードに触るための準備

goはgoで書かれているのでgoが書ければgoの処理系のコードもいじれるはずです。処理系自体に手を入れてみましょう。たとえそれをupstreamにマージしようと頑張る気が起きなくても(魔改造の定義)。とは言え処理系のコードをいじるためにも準備が必要です。

### goのコードを取ってくる

まずはgoのソースコードを持ってきましょう。この辺は惰性でやります。テキトウに1.9.2などを選んでやっていきます。

```console
# とりあえずソースコードが必要
$ git clone --depth=1 git@github.com:golang/go.git
$ go version
go version go1.9.1 linux/amd64

# 例えばgoの1.9.2を元にすすめることにする
$ cd go
$ git checkout -b go1.9.2
```

### goのビルドの方法を把握してみる

goの処理系に手を入れるために調べたことの１つ目はgoのソースからのビルドの方法です。概ね[goのドキュメントのInstalling Go from source](https://golang.org/doc/install/source)に書いてあるとおりですが。現在の環境で動いているgoでgoのソースをビルドするにはsrcのトップレベルにあるmake.bashを使います。利用したいgoの位置を`GOROOT_BOOTSTRAP`に渡して実行してください。

```console
$ GOROOT_BOOTSTRAP=`go env | grep GOROOT | cut -d = -f 2` bash -x make.bash
# 何度もbuildするときには --no-cleanをつけるとちょっとだけ早くなる
$ GOROOT_BOOTSTRAP=`go env | grep GOROOT | cut -d = -f 2` bash -x make.bash --no-clean
```

テストも含めて全部実行したい場合にはall.bashを使うと良いです(とは言え今回は一発ネタのつもりだったのでテストのメンテなどは考えてませんでした)。

また、goのビルド時には`cmd/dist/dist`が使われるのですが、このコマンドにデバッグオプションなどを与えたい場合には`GO_DISTFLAGS=-d`などしてあげると良いです。基本的にビルド時に何が行われている知りたければ、`src`以下のbashスクリプトから辿っていけば把握はできると思います。

## mapの改造のための第一歩

mapの改造自体は間に合わなかったのですが、ここからは間に合わなかっなりのの途中経過の報告を兼ねてgoのmapの実装についての紹介をしていこうと思います。

### goのmapの実装場所はどこ？

そもそもgoのmapはどのファイルで実装されているのでしょう？調べるのにはエラーメッセージやパニックメッセージを見るのが早いことが多いです。早速パニックさせてみましょう。たとえば禁止されている同じmapに排他制御なしで並行にアクセスして書き換えたりしてます。

```go
package main

import (
	"fmt"
	"sync"
)

func main() {
	m := map[int]int{}
	var wg sync.WaitGroup

    // 雑にmに並行にアクセスする
    wg.Add(1)
	go func() {
		for j := 0; j < 10000; j++ {
			m[0]++
		}
	}()
    wg.Add(1)
	go func() {
		for j := 0; j < 10000; j++ {
			m[0]++
		}
	}()
	wg.Wait()
	fmt.Println(m)
}
```

期待通りエラーがでました。以下の様な感じのメッセージが表示されています。

```console
$ go run 00broken/main.go
fatal error: concurrent map writes
```

このエラーメッセージでおもむろにgrepしてみましょう。それがmapの実装場所の候補です。個人的にはどこから読み進めるか最初の一歩を決めるときに盛大にクラッシュさせてそのときのスタックトレースだったりエラーメッセージだったりを見たりgrepしたりするということを良くします。

```console
$ git grep "concurrent map writes" src
src/cmd/vendor/github.com/google/pprof/internal/report/testdata/sample.bin
src/runtime/crash_test.go
src/runtime/hashmap.go
src/runtime/hashmap_fast.go
```

たぶん、[runtime/hashmap.go](https://golang.org/src/runtime/hashmap.go)がmapの本体ですね。高速化などのためにhashmap_fast.goで特定の型に対しての実装も提供されていたりするのでしょう。その他の２つに関してはノイズっぽいので無視します。

### 勘で探り当てたファイルを覗いてみる

hashmap.goを覗いてみましょう。内部的にはすごく素朴なhashtableの実装です(少なくとも初見では素朴な実装に見えていた)。よくある感じのバケットを幾つか用意して衝突したらチェインして後ろの方につなげていく的なあれです。

パット見た感じでは並行でのアクセス用のチェック機構などが入っていたり、サイズの拡大・縮小の最中でのイテレーションだったりのあたりが辛かったりしそうな感じです。あとunsafe.Pointerとuintptrを元にしたアドレス演算が乱れ飛ぶあたりは下層のパッケージの趣があります。

ちなみに以下の様なコメントがあるのでmapの実装場所である可能性が高いです。

```
// This file contains the implementation of Go's map type.
//
// A map is just a hash table. The data is arranged
// into an array of buckets. Each bucket contains up to
// 8 key/value pairs. The low-order bits of the hash are
// used to select a bucket. Each bucket contains a few
// high-order bits of each hash to distinguish the entries
// within a single bucket.
//
// If more than 8 keys hash to a bucket, we chain on
// extra buckets.
```

:warning: ここを読んでいるかたへの注意

ここで英文を見てみてhash tableの意味が分からなければどこかで調べたほうが良いかもしれません。バケットという言葉などがこれからいきなり出てくるのでわからなくなる恐れがあります。

わからないかたのために補足しておくには連想配列というものの利点について説明する必要があるかもしれません。連想配列はジレンマに対する妥協点の1つです(発明という捉え方もあるかもしれません)。密な配列は空間効率とアクセスの効率が良いが、たくさんの種類の疎な値の集合を同様の理屈で配列に格納しようとするとメモリー消費がひどくおおきくなる。一方でインデックスをなしにして疎な値をそのまま持った場合はメモリーの消費は抑えられるが線形探索なりになりアクセスの効率が落ちる。この良いとこ取りとまではいかなくても妥協点を探したいみたいな感じです。そしてハッシュテーブルは連想配列を提供するための方法の１つです。

```python
# 密な配列の例
L = [2,4,6,8,10,12,14,16,18]
# アクセスはO(1)
L[3] # -> 6 (2 x 3 = 6)

# 疎な配列の例
x = ["apple", "banana", "orange", "green apple", "pineapple", "pine"]
# 探索はO(N)
for x in fruits:
    if x == "pine":
        print("hmm", x)
```

(この記事はpyhackで書かれたのでなぜか唐突にpythonが登場する)

### 順序を保持したmapを考える前にmapのイテレーションの仕組みを把握しておきたい

特にgoのmapのイテレーションの部分に注目してみてみることにしましょう。runtime/hashmap.goで出てくる主要な構造体は以下です。

- hmap
- bmap
- hiter

hmapがmapの本体で、bmapはバケットの格納場所、イテレーションのときにはhiterが使われます。

例えば以下のようなコードは内部ではどのように翻訳されているのでしょうか？

```go
m := map[string]int{
    "f": 1,
    "g": 2,
    "h": 3,
}
for k, v := range m {
    fmt.Printf("%s %d ", k, v)
}
```


良いやり方があります。goにコンパイルされる時に出力されるコードを覗いてみることです。今回は `-S` オプションを使います。
こういう説明ともに定義されているオプションです。

```
flag.BoolVar(&Debug_asm, "S", false, "print assembly listing")
```

このオプションの存在もまた内部のコードを読んでいる内に知ったのですが（本当はドキュメントなどを参照してたどり着くのが正道な気もします）。goのコンパイラーにいくつかのオプションを渡せます(`go run --gcflags "-S"` みたいな形)。渡せるオプションの例は[`cmd/compile/internal/gc/main.go`](https://golang.org/src/cmd/compile/internal/gc/main.go)などを覗いてみると良いです。(ところでここでのgcはおそらくgarbage collectionの略ではなさそうな感じです。一瞬困惑したりしました。実際のところはコード生成的な意味合いみたいです)

#### 寄り道 : Debugフラグの取り扱いについて

このcompileに渡されるオプションに対応したフラグの管理にcharの配列を使うというあたりにgoに対する素朴さというかC言語っぽい何かみたいなものを感じました。

cmd/compile/internal/gc/go.go

```go
var Debug [256]int

// 以下の様にして使われる(parse時)
objabi.Flagcount("W", "debug parse tree after type checking", &Debug['W'])

// gc/walk.goなどで

// treeをparseしたあとAST的なものを出力する
if Debug['w'] != 0 && n != nil {
	Dump("walk", n)
}
```

文字はintなのでASCIIの範囲の配列を作っておけばみたいなやつですね。

### 出力されたコードからつかわれたhashmap.goの関数を覗いてみる

寄り道から戻って、compileをしてみましょう。`-S`オプションを使います。良い感じにアセンブリと一緒に対応する関数の呼び出しの情報も表示してくれます。

これは実はgo runなどにも渡せたりします。以下の様な感じでテキトウなmapのコード(冒頭のf,g,hを出力したコード)の実行例見てみましょう。

```go
$ go run --gcflags "-S" 10introduction/main.go

// 特にこのような部分のコードがみたい
//	m := map[string]int{
//		"f": 1,
//		"g": 2,
//		"h": 3,
//	}
//	for k, v := range m {
//		fmt.Printf("%s %d ", k, v)
//	}
```

以降の引用部分は`-S`による出力のうち一部分だけを抜き出した値です。

#### mapの生成

おそらくこれはmapの生成部分です。ところでhashmapの実装がruntimeパッケージにありそうという予測は正しそうですね。

```
	rel 50+4 t=15 type.map[string]int+0
	rel 86+4 t=8 runtime.makemap+0
```

#### mapへの代入

これは代入関係の部分でしょう(`m["f"] = 1"` などのような)。(runtime.mapassignではなくruntimemapassign_faststrという関数が使われていますね。この分岐はどこで行われているのでしょうか？)

```
	rel 103+4 t=15 type.map[string]int+0
	rel 119+4 t=15 go.string."f"+0
	rel 138+4 t=8 runtime.mapassign_faststr+0

	rel 157+4 t=15 type.map[string]int+0
	rel 178+4 t=15 go.string."g"+0
	rel 197+4 t=8 runtime.mapassign_faststr+0

	rel 216+4 t=15 type.map[string]int+0
	rel 237+4 t=15 go.string."h"+0
	rel 256+4 t=8 runtime.mapassign_faststr+0
```

ちなみに同じ`-S`の部分の結果の別の部分をみた方がわかりやすいかもしれません。CALLのあたり。

```
 CX
		0x006b 00107 (~/10introduction/main.go:7)        MOVQ    CX, (SP)
        0x006f 00111 (~/10introduction/main.go:7)        MOVQ    AX, 8(SP)
        0x0074 00116 (~/10introduction/main.go:7)        LEAQ    go.string."f"(SB), DX
        0x007b 00123 (~/10introduction/main.go:7)        MOVQ    DX, 16(SP)
        0x0080 00128 (~/10introduction/main.go:7)        MOVQ    $1, 24(SP)
        0x0089 00137 (~/10introduction/main.go:7)        PCDATA  $0, $1
        0x0089 00137 (~/10introduction/main.go:7)        CALL    runtime.mapassign_faststr(SB)
```


runtime.mapassign_faststr()が代入時に呼ばれていることがもう少し直接的にわかりそうです。

#### mapのループに使う部分

ここか本題でおそらくループに使う処理の部分です。runtime.mapiterinitで何かが初期化されruntimeruntime.mapiternextで次の値を取り出すiteratorっぽいオブジェクトがいそうなことが予想できます(これが以前紹介したhiterです)。

```
	rel 298+4 t=8 runtime.duffzero+273
	rel 309+4 t=15 type.map[string]int+0
	rel 341+4 t=8 runtime.mapiterinit+0

	rel 422+4 t=15 type.string+0
	rel 441+4 t=8 runtime.convT2Estring+0
	rel 468+4 t=15 type.int+0
	rel 487+4 t=8 runtime.convT2E64+0
	rel 514+4 t=15 go.string."%s %d "+0
	rel 560+4 t=8 fmt.Printf+0
	rel 577+4 t=8 runtime.mapiternext+0
```

### hashmap.goのコードに戻る

さて出力結果も見終わったことですし、このへんでhashmap.goに戻ってみてみましょう。無事initとnextを見つけます。

```go
// mapiterinit initializes the hiter struct used for ranging over maps.
// The hiter struct pointed to by 'it' is allocated on the stack
// by the compilers order pass or on the heap by reflect_mapiterinit.
// Both need to have zeroed hiter since the struct contains pointers.
func mapiterinit(t *maptype, h *hmap, it *hiter) {
...
}

func mapiternext(it *hiter) {
}
```

イテレーションにはhiterが使われそうで、initは一回だけ実行されるみたいな感じになりそうです。hiterにはkeyとvalueというフィールドが用意されているのでこれがループ時に触る`for k, v := range m ` の `k` と `v` の値になりそうです。

hiterの構造体の定義の一部を抜粋

```go
// A hash iteration structure.
// If you modify hiter, also change cmd/internal/gc/reflect.go to indicate
// the layout of this structure.
type hiter struct {
	key         unsafe.Pointer // Must be in first position.  Write nil to indicate iteration end (see cmd/internal/gc/range.go).
	value       unsafe.Pointer // Must be in second position (see cmd/internal/gc/range.go).
...
}
```

#### mapのイテレーションの順序は順不同(あえてやっています)

mapのイテレーションの順序が順不同というのは常識かもというかよく聞く話ですが(ちなみにこれはgoがあえてそうやっているところです)。実際にはどうやっているのでしょう？これはhiterとhmapのそれぞれで乱数を使っているを覗くとわかります。具体的にはhmap中ではハッシュ値に対するシード(ソルト)的なものをhmap.hash0で生成してハッシュ値を得る時に常に使うようにし。同一のmapのイテレーションに対しても順不同を実現するためにhiterで最初に選択するbucketの位置を決めるのに乱数を使っています。いずれもfast_rand(xorshift64+)と呼ばれるちょっと特殊な関数のようです(どういうものかはあんまり真面目に理解していない)。

呼ばれている関数(短いがヤバそうな雰囲気がある)

```go
//go:nosplit
func fastrand() uint32 {
	mp := getg().m
	// Implement xorshift64+: 2 32-bit xorshift sequences added together.
	// Shift triplet [17,7,16] was calculated as indicated in Marsaglia's
	// Xorshift paper: https://www.jstatsoft.org/article/view/v008i14/xorshift.pdf
	// This generator passes the SmallCrush suite, part of TestU01 framework:
	// http://simul.iro.umontreal.ca/testu01/tu01.html
	s1, s0 := mp.fastrand[0], mp.fastrand[1]
	s1 ^= s1 << 17
	s1 = s1 ^ s0 ^ s1>>7 ^ s0>>16
	mp.fastrand[0], mp.fastrand[1] = s0, s1
	return s0 + s1
}
```


とは言えほどほどにコストがかからずあまり偏った数値がでない位の感じの軽めの乱数なような気がします(推測です)。

## hashmap.goでの具体的な乱数の使い方について

hashmapでの乱数の利用についてもう少し詳しく見ていきます(間に合わなかった魔改造版ではこいつと良い感じにやっていきたかった)。

### hmap生成時にソルトとして利用

生成時にhash0が設定され(`h.hash0 = fastrand()`)。

```go
func makemap(t *maptype, hint int, h *hmap) *hmap {
...

	// initialize Hmap
	if h == nil {
		h = (*hmap)(newobject(t.hmap))
	}
	h.hash0 = fastrand()

...
	return h
}
```

### hmapアクセス時ソルトを必ず利用

mapにアクセスする際にhash0を常に常にソルト(シードという言葉のほうが適切？)として使っています(`hash := alg.hash(key, uintptr(h.hash0))`)。

```
func mapaccess1(t *maptype, h *hmap, key unsafe.Pointer) unsafe.Pointer {
... 簡略化したコード

	alg := t.key.alg
	hash := alg.hash(key, uintptr(h.hash0))

	bucket := hash & bucketMask(h.B)
	b := (*bmap)(unsafe.Pointer(uintptr(h.buckets) + bucket*uintptr(t.bucketsize)))
	top := tophash(hash)

	for ; b != nil; b = b.overflow(t) {
		for i := uintptr(0); i < bucketCnt; i++ {
			if b.tophash[i] != top {
				continue
			}
			k := add(unsafe.Pointer(b), dataOffset+i*uintptr(t.keysize))
			if alg.equal(key, k) {
				v := add(unsafe.Pointer(b), dataOffset+bucketCnt*uintptr(t.keysize)+i*uintptr(t.valuesize))
				return v
			}
		}
	}
...
}
```

#### 寄り道 : 以前のアドレス値とハッシュ値の利用方法について

ちなみにhashmapを使う時にハッシュ値を計算する必要がありますが。上位の方はバケットの選択に。下位の方は値の一致に使っています。

(実は[hashmap.go](https://golang.org/src/runtime/hashmap.go)のファイルの冒頭に書いてあるのでした)

```
// A map is just a hash table. The data is arranged
// into an array of buckets. Each bucket contains up to
// 8 key/value pairs. The low-order bits of the hash are
// used to select a bucket. Each bucket contains a few
// high-order bits of each hash to distinguish the entries
// within a single bucket.
```

バケットの選択というのはこの部分で

```go
	bucket := hash & bucketMask(h.B)
	b := (*bmap)(unsafe.Pointer(uintptr(h.buckets) + bucket*uintptr(t.bucketsize)))
	top := tophash(hash)
```


値の一致というのはここの部分のことです。

```go
			if alg.equal(key, k) {
				v := add(unsafe.Pointer(b), dataOffset+bucketCnt*uintptr(t.keysize)+i*uintptr(t.valuesize))
				return v
			}
```


ここでのalgとは何かというと察しの方もいると思いますが特定の型ごとに定義された良い感じに計算してくれる何かです。実際には`runtime/alg.go` あたりにあります。

```go
func memequal32(p, q unsafe.Pointer) bool {
	return *(*int32)(p) == *(*int32)(q)
}

func int32Hash(i uint32, seed uintptr) uintptr {
	return algarray[alg_MEM32].hash(noescape(unsafe.Pointer(&i)), seed)
}

var algarray = [alg_max]typeAlg{
...
	alg_MEM32:    {memhash32, memequal32},
	alg_MEM64:    {memhash64, memequal64},
..
	alg_STRING:   {strhash, strequal},
}

```

特殊化のことについて触れることができれば後にこのことについても詳しく書いてみようと思います(寄り道終わり)

### hiterでのイテレーション時

そしてイテレーションの部分では最初のバケットの選択に使われています(`r := uintptr(fastrand())`)。

```go
func mapiterinit(t *maptype, h *hmap, it *hiter) {
...

	// decide where to start
	r := uintptr(fastrand())
	if h.B > 31-bucketCntBits {
		r += uintptr(fastrand()) << 31
	}
	it.startBucket = r & bucketMask(h.B)
	it.offset = uint8(r >> h.B & (bucketCnt - 1))

	// iterator state
	it.bucket = it.startBucket

...
}
```

初期化後のイテレーションの繰り返しの際(mapiternext)には順序は特に変わることなく、バケットを順々に走査していく感じです（読み間違いをしていなければ）。

ところでhashmap.goのコードを読んでいたら副次的にunsafe.Pointerの使いかた(の一部)もわかってきてしまいました(怖そうなので近づかないようにしていたのに。。)。

## 今度は構文の方から

hashmap.goの実装がわかったところで今度は構文の方からどういうことが行われるのかを見ていきましょう。ビルド時にあれこれ見ていたgc付近には[`gc/walk.go`](https://golang.org/src/cmd/compile/internal/gc/walk.go)という名前のファイルもありました。

そしてhashmap.goのhmapなどの定義のコメントにはいくつか気になることが書かれています。[`cmd/internal/gc/reflect.go`](https://golang.org/src/cmd/compile/internal/gc/reflect.go)というファイルのコードと一致させる必要があるようです。

```go
// A header for a Go map.
type hmap struct {
	// Note: the format of the Hmap is encoded in ../../cmd/internal/gc/reflect.go and
	// ../reflect/type.go. Don't change this structure without also changing that code!
...
}
```


次の焦点は`cmd/internal/gc`付近になりそうです。

### hmapの作成はhmapという関数で

１つコメントの通りに[reflect.go](https://golang.org/src/cmd/compile/internal/gc/reflect.go)を覗いてみましょう。runtime/hashmap.goのhmapの構造体に対応するようなhmap()関数が定義されています。
単純に言えば、型のような情報を持った値を構文を走査(walk)している最中に作るという感じです。

(間に合わなかったことに対する恨みつらみをコメントに追加しています)

```go
// hmap builds a type representing a Hmap structure for the given map type.
// Make sure this stays in sync with ../../../../runtime/hashmap.go.
func hmap(t *types.Type) *types.Type {
    // 再帰的な走査の場合にこれがないと無限再帰に陥る。(これを新しく自分で追加しようとした型に忘れてハマったりしていたのも間に合わなかった原因の一つ)
    // これを忘れたときの振る舞いは面白く、静かに無限にメモリーを食べていきます
	if t.MapType().Hmap != nil {
		return t.MapType().Hmap
	}

    // 型定義に対応したフィールド設定(本当はもう少したくさんある)

	// build a struct:
	// type hmap struct {
...
	//    hash0      uint32
	//    buckets    *bmap
	//    oldbuckets *bmap  // oldとbucketsがあるのはサイズを伸張中でのイテレーションみたいなときに古い方もほしいので
...
	// }
	bmap := bmap(t)
	fields := []*types.Field{
...
		makefield("hash0", types.Types[TUINT32]), // Used in walk.go for OMAKEMAP.
		makefield("buckets", types.NewPtr(bmap)), // Used in walk.go for OMAKEMAP.
		makefield("oldbuckets", types.NewPtr(bmap)),
...
	}

	hmap := types.New(TSTRUCT)
	hmap.SetNoalg(true)
	hmap.SetFields(fields)
	dowidth(hmap)

    // ここに各種チェックがあったりする


    // この辺の記述を間違えると比較(cmp)のときにバグる
	t.MapType().Hmap = hmap
	hmap.StructType().Map = t
	return hmap
}
```

### 構文をparseして対応した方のOMAKEMAPでhmapを生成

ところでこのreflect.goで使われるhmapなどはどこで使われるのかというと。`gc/walk.go`のwalkexpr()という以下にもな名前の関数の中での分岐に使われます。各ノードに対応した処理が埋め込まれます。型情報が手に入るのでわーいヤッターという感じでhashmap.goでは確保するメモリのサイズが決められるようになります。


```go
// The result of walkexpr MUST be assigned back to n, e.g.
// 	n.Left = walkexpr(n.Left, init)
func walkexpr(n *Node, init *Nodes) *Node {

// nodeの持つ操作で分岐
	switch n.Op {
...
	case OMAKEMAP:
		t := n.Type
		hmapType := hmap(t)
		hint := n.Left
        .. 型チェックとか色々やる
...
    }
}
```

### イテレーションの始まりは `ORANGE`

イテレーションもどこかにあるはずで、walk.goからwalkrange()が呼ばれます。

```go
func walkstmt(n *Node) *Node {
	if n == nil {
		return n
	}

	switch n.Op {
..
	case ORANGE:
		n = walkrange(n)
	}
}
```

walkrange()は同じパッケージの[range.go](https://golang.org/src/cmd/compile/internal/gc/range.go)にあります。 walkrange()の中ではやっぱり型によって分岐が発生し良い感じのコードに変換されます(気持ちだけを伝えたいためにすごく省略しているので本来のコードが知りたい場合はリンク先のコードを見てみてください)。

```go
func walkrange(n *Node) *Node {

	t := n.Type
...

	switch t.Etype {
...
	case TMAP: // mapの時!!

.. //ぐちゃぐちゃとやっているけれど実質はこういう構文に変換したいということ
   for mapiterinit(t, h, hiter); hiter.key != nil; mapiternext(hiter) {
   }
...
}
```

イテレーションの方はなんとなくわかりましたね。

### 高速化のための特殊化も実は結構単純な方法で分岐

そして特殊化の話についてようやく触れます。時々至るところに現れた `_faststr` とか `_fast32` みたいなsuffixの話です。これも結局のところwalk.goの`walkexpr()`の付近での出来事です。

```go
func walkexpr(n *Node, init *Nodes) *Node {

	switch n.Op {
...
    case OINDEXMAP: // こういうのがある
        // Replace m[k] with *map{access1,assign}(maptype, m, &k)
		map_ := n.Left
		t := map_.Type

        // 本当は色々分岐がある
		fast := mapfast(t)
    	n = mkcall1(mapfn(mapassign[fast], t), nil, init, typename(t), map_, key)
    }
}
```

ここの部分実は読むのがちょっと大変なのですが。mapfnが呼び出す関数の分岐を行っています。

```go
func mapfn(name string, t *types.Type) *Node {
	if !t.IsMap() {
		Fatalf("mapfn %v", t)
	}
	fn := syslook(name)
	fn = substArgTypes(fn, t.Key(), t.Val(), t.Key(), t.Val())
	return fn
}
```

まず`syslook()`は`Runtimepkg.Lookup(name)`になり、結局のところruntimeに定義されているメンバーを関数名で検索します。
(つまり、mapfnに渡された時点で`mapaccess1`が`mapaccess1_fast32`であったりするということです)

ここでの分岐は単純でそのままenum 的なものを定義して型ディスパッチし、型から関数名を生成しているだけです(ランタイム時には動的な言語で結構やったりしますね)。

```go
const (
	mapslow = iota
	mapfast32
	mapfast64
	mapfaststr
	nmapfast
)

type mapnames [nmapfast]string

func mkmapnames(base string) mapnames {
	return mapnames{base, base + "_fast32", base + "_fast64", base + "_faststr"}
}

var mapaccess1 = mkmapnames("mapaccess1")
var mapaccess2 = mkmapnames("mapaccess2")
var mapassign = mkmapnames("mapassign")
var mapdelete = mkmapnames("mapdelete")

func mapfast(t *types.Type) int {
	// Check ../../runtime/hashmap.go:maxValueSize before changing.
	if t.Val().Width > 128 {
		return mapslow
	}
	switch algtype(t.Key()) {
	case AMEM32:
		return mapfast32
	case AMEM64:
		return mapfast64
	case ASTRING:
		return mapfaststr
	}
	return mapslow
}
```

## おわりに

本当はgoの魔改造を間に合わせたかったのですが。間に合わなかったのでその過程で得た情報を記事にすることにしました。
ちなみになぜ魔改造をしようと思うかというのはただただ楽しそうと思ったからであるし。
役に立つとかそういような不要な感情はお仕事をやるときだけで十分だという気持ちの現れでもあります。

goはgo自身で書かれているしオープンソースなのでgo自体のソースコードをいじるのも遊びとしては結構良いです。

追伸

冒頭で出てきたgo-swaggerについて、OAS3.0(現在はOAS2.0)対応のついでに内部でmapを使わずやるみたいな話が出ているようです。

- https://github.com/go-swagger/go-swagger/issues/1122

そして実用上は生成されたコードはPull Request上のdiffから除外できるのであんまり困って無いです。
トリに格納するかしないかは判断に分かれるところですが。waculではリポジトリに生成されたコードも格納しています。

waculでは[go-swagger](https://github.com/go-swagger/go-swagger)でコード生成をしています。go製のツールです。go-swaggerについては、APIの仕様ドキュメントをJSON/YAMLで記述しておくと良い感じに実行可能なドキュメント環境になったり複数の言語間でのサーバー/クライアント部分のコードを生成してくれたりする位の雑な理解で良いです。何かファイルを渡すとコードを生成してくれるというところが重要です。

### go-swaggerの出力は順序が毎回バラバラ

さてそのようなgo-swaggerによるコード生成ですが、現在のバージョンでは同じドキュメントを元にコード生成を行おうとしても生成結果に差分が出てきてしまいます。論理的には同じコードなのですが、関数や構造体などの位置が微妙に変わってきてしまいます。例えば、あるパッケージAにf,g,hという順で出力される場合とg,f,hという順で出力する場合などにそれぞれの出力位置が前後したり変わってしまったりして不要な差分が出てしまったりします。

あるときは

```go
func f() {
...
}
func g() {
...
}
func h() {
...
}
```

またあるときは

```go
func g() {
...
}
func f() {
...
}
func h() {
...
}
```


### goのmapに順序なんてあるわけない

これは内部的にはgoのmapを直接利用しているせいです。goのmapをイテレートしたときの順序が一定ではないということが原因です。例えば以下の様なmapをイテレートするようなコードの実行結果は一定にはなりません(あとで詳しく説明しますがgoがあえてそうしています)。

```go
package main

import "fmt"

func main() {
	m := map[string]int{
		"f": 1,
		"g": 2,
		"h": 3,
	}
	for k, v := range m {
		fmt.Printf("%s %d ", k, v)
	}
	fmt.Println()
}
```

ためしに100回位実行してみた結果が以下ですばらつきがありますね。

```console
$ for i in `seq 100`; do go run main.go >> output.txt; done
$ cat output.txt | sort | uniq -c
     76 f 1 g 2 h 3 
     10 g 2 h 3 f 1 
     14 h 3 f 1 g 2 
```

### 魔改造したい

ここで魔改造です。

> goのmapをforでループした結果が順不同になるのがだめなら、順序を保持したmapを利用するように魔改造してしまえば出力が一定になるのでは？という素朴な思いつきを元に魔改造をしてみることにしてみました。

当初は、以上のような導入文で始める記事を書こうというつもりで、魔改造したgoを作ろうと思っていたのですが、間に合いませんでした。間に合いませんでしたたのでその途中でわかったことなどをまとめようと思います。後半戦に続くみたいな感じになるかもしれません。

1日(24時間)くらいあれば無理矢理にでも実装できるかな−と思ったのですが。全然足りなかった感じでした。

## 魔改造のための第一歩goのコードに触るための準備

goはgoで書かれているのでgoが書ければgoの処理系のコードもいじれるはずです。処理系自体に手を入れてみましょう。たとえそれをupstreamにマージしようと頑張る気が起きなくても(魔改造の定義)。とは言え処理系のコードをいじるためにも準備が必要です。

### goのコードを取ってくる

まずはgoのソースコードを持ってきましょう。この辺は惰性でやります。テキトウに1.9.2などを選んでやっていきます。

```console
# とりあえずソースコードが必要
$ git clone --depth=1 git@github.com:golang/go.git
$ go version
go version go1.9.1 linux/amd64

# 例えばgoの1.9.2を元にすすめることにする
$ cd go
$ git checkout -b go1.9.2
```

### goのビルドの方法を把握してみる

goの処理系に手を入れるために調べたことの１つ目はgoのソースからのビルドの方法です。概ね[goのドキュメントのInstalling Go from source](https://golang.org/doc/install/source)に書いてあるとおりですが。現在の環境で動いているgoでgoのソースをビルドするにはsrcのトップレベルにあるmake.bashを使います。利用したいgoの位置を`GOROOT_BOOTSTRAP`に渡して実行してください。

```console
$ GOROOT_BOOTSTRAP=`go env | grep GOROOT | cut -d = -f 2` bash -x make.bash
# 何度もbuildするときには --no-cleanをつけるとちょっとだけ早くなる
$ GOROOT_BOOTSTRAP=`go env | grep GOROOT | cut -d = -f 2` bash -x make.bash --no-clean
```

テストも含めて全部実行したい場合にはall.bashを使うと良いです(とは言え今回は一発ネタのつもりだったのでテストのメンテなどは考えてませんでした)。

また、goのビルド時には`cmd/dist/dist`が使われるのですが、このコマンドにデバッグオプションなどを与えたい場合には`GO_DISTFLAGS=-d`などしてあげると良いです。基本的にビルド時に何が行われている知りたければ、`src`以下のbashスクリプトから辿っていけば把握はできると思います。

## mapの改造のための第一歩

mapの改造自体は間に合わなかったのですが、ここからは間に合わなかっなりのの途中経過の報告を兼ねてgoのmapの実装についての紹介をしていこうと思います。

### goのmapの実装場所はどこ？

そもそもgoのmapはどのファイルで実装されているのでしょう？調べるのにはエラーメッセージやパニックメッセージを見るのが早いことが多いです。早速パニックさせてみましょう。たとえば禁止されている同じmapに排他制御なしで並行にアクセスして書き換えたりしてます。

```go
package main

import (
	"fmt"
	"sync"
)

func main() {
	m := map[int]int{}
	var wg sync.WaitGroup

    // 雑にmに並行にアクセスする
    wg.Add(1)
	go func() {
		for j := 0; j < 10000; j++ {
			m[0]++
		}
	}()
    wg.Add(1)
	go func() {
		for j := 0; j < 10000; j++ {
			m[0]++
		}
	}()
	wg.Wait()
	fmt.Println(m)
}
```

期待通りエラーがでました。以下の様な感じのメッセージが表示されています。

```console
$ go run 00broken/main.go
fatal error: concurrent map writes
```

このエラーメッセージでおもむろにgrepしてみましょう。それがmapの実装場所の候補です。個人的にはどこから読み進めるか最初の一歩を決めるときに盛大にクラッシュさせてそのときのスタックトレースだったりエラーメッセージだったりを見たりgrepしたりするということを良くします。

```console
$ git grep "concurrent map writes" src
src/cmd/vendor/github.com/google/pprof/internal/report/testdata/sample.bin
src/runtime/crash_test.go
src/runtime/hashmap.go
src/runtime/hashmap_fast.go
```

たぶん、[runtime/hashmap.go](https://golang.org/src/runtime/hashmap.go)がmapの本体ですね。高速化などのためにhashmap_fast.goで特定の型に対しての実装も提供されていたりするのでしょう。その他の２つに関してはノイズっぽいので無視します。

### 勘で探り当てたファイルを覗いてみる

hashmap.goを覗いてみましょう。内部的にはすごく素朴なhashtableの実装です(少なくとも初見では素朴な実装に見えていた)。よくある感じのバケットを幾つか用意して衝突したらチェインして後ろの方につなげていく的なあれです。

パット見た感じでは並行でのアクセス用のチェック機構などが入っていたり、サイズの拡大・縮小の最中でのイテレーションだったりのあたりが辛かったりしそうな感じです。あとunsafe.Pointerとuintptrを元にしたアドレス演算が乱れ飛ぶあたりは下層のパッケージの趣があります。

ちなみに以下の様なコメントがあるのでmapの実装場所である可能性が高いです。

```
// This file contains the implementation of Go's map type.
//
// A map is just a hash table. The data is arranged
// into an array of buckets. Each bucket contains up to
// 8 key/value pairs. The low-order bits of the hash are
// used to select a bucket. Each bucket contains a few
// high-order bits of each hash to distinguish the entries
// within a single bucket.
//
// If more than 8 keys hash to a bucket, we chain on
// extra buckets.
```

:warning: ここを読んでいるかたへの注意

ここで英文を見てみてhash tableの意味が分からなければどこかで調べたほうが良いかもしれません。バケットという言葉などがこれからいきなり出てくるのでわからなくなる恐れがあります。

わからないかたのために補足しておくには連想配列というものの利点について説明する必要があるかもしれません。連想配列はジレンマに対する妥協点の1つです(発明という捉え方もあるかもしれません)。密な配列は空間効率とアクセスの効率が良いが、たくさんの種類の疎な値の集合を同様の理屈で配列に格納しようとするとメモリー消費がひどくおおきくなる。一方でインデックスをなしにして疎な値をそのまま持った場合はメモリーの消費は抑えられるが線形探索なりになりアクセスの効率が落ちる。この良いとこ取りとまではいかなくても妥協点を探したいみたいな感じです。そしてハッシュテーブルは連想配列を提供するための方法の１つです。


```python
# 密な配列の例
L = [2,4,6,8,10,12,14,16,18]
# アクセスはO(1)
L[3] # -> 6 (2 x 3 = 6)

# 疎な配列の例
x = ["apple", "banana", "orange", "green apple", "pineapple", "pine"]
# 探索はO(N)
for x in fruits:
    if x == "pine":
        print("hmm", x)
```

### 順序を保持したmapを考える前にmapのイテレーションの仕組みを把握しておきたい

特にgoのmapのイテレーションの部分に注目してみてみることにしましょう。runtime/hashmap.goで出てくる主要な構造体は以下です。

- hmap
- bmap
- hiter

hmapがmapの本体で、bmapはバケットの格納場所、イテレーションのときにはhiterが使われます。

例えば以下ようなコードは内部ではどのように翻訳されているのでしょうか？

```go
m := map[string]int{
    "f": 1,
    "g": 2,
    "h": 3,
}
for k, v := range m {
    fmt.Printf("%s %d ", k, v)
}
```


良いやり方があります。goにコンパイルされる時に出力されるコードを覗いてみることです。今回は `-S` オプションを使います。

```
flag.BoolVar(&Debug_asm, "S", false, "print assembly listing")
```

これもまた内部のコードを読んでいる内に知ったのですが（本当はドキュメントなどを参照してたどり着くのが正道な気もします）。goのコンパイラーにいくつかのオプションを渡せます。渡せるオプションの例は[`cmd/compile/internal/gc/main.go`](https://golang.org/src/cmd/compile/internal/gc/main.go)などを覗いてみると良いです。(ところでここでのgcはおそらくgarbage collectionの略ではなさそうな感じです。一瞬困惑したりしました。実際のところはコード生成的な意味合いみたいです)

#### 寄り道 : Debugフラグの取り扱いについて

このcompileに渡されるオプションに対応したフラグの管理にcharの配列を使うというあたりにgoに対する素朴さというかC言語っぽい何かみたいなものを感じました。

cmd/compile/internal/gc/go.go

```go
var Debug [256]int

// 以下の様にして使われる(parse時)
objabi.Flagcount("W", "debug parse tree after type checking", &Debug['W'])

// gc/walk.goなどで

// treeをparseしたあとAST的なものを出力する
if Debug['w'] != 0 && n != nil {
	Dump("walk", n)
}
```

文字はintなのでASCIIの範囲の配列を作っておけばみたいなやつですね。

### 出力されたコードからつかわれたhashmap.goの関数を覗いてみる

寄り道から戻って、compileをしてみましょう。`-S`オプションを使います。良い感じにアセンブリと一緒に対応する関数の呼び出しの情報も表示してくれます。

これは実はgo runなどにも渡せたりします。以下の様な感じでテキトウなmapのコード(冒頭のf,g,hを出力したコード)の実行例見てみましょう。

```go
$ go run --gcflags "-S" 10introduction/main.go

// 特にこのような部分のコードがみたい
//	m := map[string]int{
//		"f": 1,
//		"g": 2,
//		"h": 3,
//	}
//	for k, v := range m {
//		fmt.Printf("%s %d ", k, v)
//	}
```

以降の引用部分は`-S`による出力のうち一部分だけを抜き出した値です。

#### mapの生成

おそらくこれはmapの生成部分です。ところでhashmapの実装がruntimeパッケージにありそうという予測は正しそうですね。

```
	rel 50+4 t=15 type.map[string]int+0
	rel 86+4 t=8 runtime.makemap+0
```

#### mapへの代入

これは代入関係の部分でしょう(`m["f"] = 1"` などのような)。(runtime.mapassignではなくruntimemapassign_faststrという関数が使われていますね。この分岐はどこで行われているのでしょうか？)
```
	rel 103+4 t=15 type.map[string]int+0
	rel 119+4 t=15 go.string."f"+0
	rel 138+4 t=8 runtime.mapassign_faststr+0

	rel 157+4 t=15 type.map[string]int+0
	rel 178+4 t=15 go.string."g"+0
	rel 197+4 t=8 runtime.mapassign_faststr+0

	rel 216+4 t=15 type.map[string]int+0
	rel 237+4 t=15 go.string."h"+0
	rel 256+4 t=8 runtime.mapassign_faststr+0
```

ちなみに同じ`-S`の部分の結果の別の部分をみた方がわかりやすいかもしれません。CALLのあたり。

```
 CX
		0x006b 00107 (~/10introduction/main.go:7)        MOVQ    CX, (SP)
        0x006f 00111 (~/10introduction/main.go:7)        MOVQ    AX, 8(SP)
        0x0074 00116 (~/10introduction/main.go:7)        LEAQ    go.string."f"(SB), DX
        0x007b 00123 (~/10introduction/main.go:7)        MOVQ    DX, 16(SP)
        0x0080 00128 (~/10introduction/main.go:7)        MOVQ    $1, 24(SP)
        0x0089 00137 (~/10introduction/main.go:7)        PCDATA  $0, $1
        0x0089 00137 (~/10introduction/main.go:7)        CALL    runtime.mapassign_faststr(SB)
```


runtime.mapassign_faststr()が代入時に呼ばれていることがもう少し直接的にわかりそうです。

#### mapのループに使う部分

ここか本題でおそらくループに使う処理の部分です。runtime.mapiterinitで何かが初期化されruntimeruntime.mapiternextで次の値を取り出すiteratorっぽいオブジェクトがいそうなことが予想できます(これが以前紹介したhiterです)。

```
	rel 298+4 t=8 runtime.duffzero+273
	rel 309+4 t=15 type.map[string]int+0
	rel 341+4 t=8 runtime.mapiterinit+0

	rel 422+4 t=15 type.string+0
	rel 441+4 t=8 runtime.convT2Estring+0
	rel 468+4 t=15 type.int+0
	rel 487+4 t=8 runtime.convT2E64+0
	rel 514+4 t=15 go.string."%s %d "+0
	rel 560+4 t=8 fmt.Printf+0
	rel 577+4 t=8 runtime.mapiternext+0
```

### hashmap.goのコードに戻る

さて出力結果も見終わったことですし、このへんでhashmap.goに戻ってみてみましょう。無事initとnextを見つけます。

```go
// mapiterinit initializes the hiter struct used for ranging over maps.
// The hiter struct pointed to by 'it' is allocated on the stack
// by the compilers order pass or on the heap by reflect_mapiterinit.
// Both need to have zeroed hiter since the struct contains pointers.
func mapiterinit(t *maptype, h *hmap, it *hiter) {
...
}

func mapiternext(it *hiter) {
}
```

イテレーションにはhiterが使われそうで、initは一回だけ実行されるみたいな感じになりそうです。hiterにはkeyとvalueというフィールドが用意されているのでこれがループ時に触る`for k, v := range m ` の `k` と `v` の値になりそうです。

hiterの構造体の定義の一部を抜粋

```go
// A hash iteration structure.
// If you modify hiter, also change cmd/internal/gc/reflect.go to indicate
// the layout of this structure.
type hiter struct {
	key         unsafe.Pointer // Must be in first position.  Write nil to indicate iteration end (see cmd/internal/gc/range.go).
	value       unsafe.Pointer // Must be in second position (see cmd/internal/gc/range.go).
...
}
```

#### mapのイテレーションの順序は順不同(あえてやっています)

mapのイテレーションの順序が順不同というのは常識かもというかよく聞く話ですが(ちなみにこれはgoがあえてそうやっているところです)。実際にはどうやっているのでしょう？これはhiterとhmapのそれぞれで乱数を使っているを覗くとわかります。具体的にはhmap中ではハッシュ値に対するシード(ソルト)的なものをhmap.hash0で生成してハッシュ値を得る時に常に使うようにし。同一のmapのイテレーションに対しても順不同を実現するためにhiterで最初に選択するbucketの位置を決めるのに乱数を使っています。いずれもfast_rand(xorshift64+)と呼ばれるちょっと特殊な関数のようです(どういうものかはあんまり真面目に理解していない)。

呼ばれている関数(短いがヤバそうな雰囲気がある)

```go
//go:nosplit
func fastrand() uint32 {
	mp := getg().m
	// Implement xorshift64+: 2 32-bit xorshift sequences added together.
	// Shift triplet [17,7,16] was calculated as indicated in Marsaglia's
	// Xorshift paper: https://www.jstatsoft.org/article/view/v008i14/xorshift.pdf
	// This generator passes the SmallCrush suite, part of TestU01 framework:
	// http://simul.iro.umontreal.ca/testu01/tu01.html
	s1, s0 := mp.fastrand[0], mp.fastrand[1]
	s1 ^= s1 << 17
	s1 = s1 ^ s0 ^ s1>>7 ^ s0>>16
	mp.fastrand[0], mp.fastrand[1] = s0, s1
	return s0 + s1
}
```


とは言えほどほどにコストがかからずあまり偏った数値がでない位の感じの軽めの乱数なような気がします(推測です)。

## hashmap.goでの具体的な乱数の使い方について

hashmapでの乱数の利用についてもう少し詳しく見ていきます(間に合わなかった魔改造版ではこいつと良い感じにやっていきたかった)。

### hmap生成時にソルトとして利用

生成時にhash0が設定され(`h.hash0 = fastrand()`)。

```go
func makemap(t *maptype, hint int, h *hmap) *hmap {
...

	// initialize Hmap
	if h == nil {
		h = (*hmap)(newobject(t.hmap))
	}
	h.hash0 = fastrand()

...
	return h
}
```

### hmapアクセス時ソルトを必ず利用

mapにアクセスする際にhash0を常に常にソルト(シードという言葉のほうが適切？)として使っています(`hash := alg.hash(key, uintptr(h.hash0))`)。

```
func mapaccess1(t *maptype, h *hmap, key unsafe.Pointer) unsafe.Pointer {
... 簡略化したコード

	alg := t.key.alg
	hash := alg.hash(key, uintptr(h.hash0))

	bucket := hash & bucketMask(h.B)
	b := (*bmap)(unsafe.Pointer(uintptr(h.buckets) + bucket*uintptr(t.bucketsize)))
	top := tophash(hash)

	for ; b != nil; b = b.overflow(t) {
		for i := uintptr(0); i < bucketCnt; i++ {
			if b.tophash[i] != top {
				continue
			}
			k := add(unsafe.Pointer(b), dataOffset+i*uintptr(t.keysize))
			if alg.equal(key, k) {
				v := add(unsafe.Pointer(b), dataOffset+bucketCnt*uintptr(t.keysize)+i*uintptr(t.valuesize))
				return v
			}
		}
	}
...
}
```

#### 寄り道 : 以前のアドレス値とハッシュ値の利用方法について

ちなみにhashmapを使う時にハッシュ値を計算する必要がありますが。上位の方はバケットの選択に。下位の方は値の一致に使っています。

(実は[hashmap.go](https://golang.org/src/runtime/hashmap.go)のファイルの冒頭に書いてあるのでした)

```
// A map is just a hash table. The data is arranged
// into an array of buckets. Each bucket contains up to
// 8 key/value pairs. The low-order bits of the hash are
// used to select a bucket. Each bucket contains a few
// high-order bits of each hash to distinguish the entries
// within a single bucket.
```

バケットの選択というのはこの部分で

```go
	bucket := hash & bucketMask(h.B)
	b := (*bmap)(unsafe.Pointer(uintptr(h.buckets) + bucket*uintptr(t.bucketsize)))
	top := tophash(hash)
```


値の一致というのはここの部分のことです。

```go
			if alg.equal(key, k) {
				v := add(unsafe.Pointer(b), dataOffset+bucketCnt*uintptr(t.keysize)+i*uintptr(t.valuesize))
				return v
			}
```


ここでのalgとは何かというと察しの方もいると思いますが特定の型ごとに定義された良い感じに計算してくれる何かです。実際には`runtime/alg.go` あたりにあります。

```go
func memequal32(p, q unsafe.Pointer) bool {
	return *(*int32)(p) == *(*int32)(q)
}

func int32Hash(i uint32, seed uintptr) uintptr {
	return algarray[alg_MEM32].hash(noescape(unsafe.Pointer(&i)), seed)
}

var algarray = [alg_max]typeAlg{
...
	alg_MEM32:    {memhash32, memequal32},
	alg_MEM64:    {memhash64, memequal64},
..
	alg_STRING:   {strhash, strequal},
}

```

特殊化のことについて触れることができれば後にこのことについても詳しく書いてみようと思います(寄り道終わり)

### hiterでのイテレーション時

そしてイテレーションの部分では最初のバケットの選択に使われています(`r := uintptr(fastrand())`)。

```go
func mapiterinit(t *maptype, h *hmap, it *hiter) {
...

	// decide where to start
	r := uintptr(fastrand())
	if h.B > 31-bucketCntBits {
		r += uintptr(fastrand()) << 31
	}
	it.startBucket = r & bucketMask(h.B)
	it.offset = uint8(r >> h.B & (bucketCnt - 1))

	// iterator state
	it.bucket = it.startBucket

...
}
```

初期化後のイテレーションの繰り返しの際(mapiternext)には順序は特に変わることなく、バケットを順々に走査していく感じです（読み間違いをしていなければ）。

ところでhashmap.goのコードを読んでいたら副次的にunsafe.Pointerの使いかた(の一部)もわかってきてしまいました(怖そうなので近づかないようにしていたのに。。)。

## 今度は構文の方から

hashmap.goの実装がわかったところで今度は構文の方からどういうことが行われるのかを見ていきましょう。ビルド時にあれこれ見ていたgc付近には[`gc/walk.go`](https://golang.org/src/cmd/compile/internal/gc/walk.go)という名前のファイルもありました。

そしてhashmap.goのhmapなどの定義のコメントにはいくつか気になることが書かれています。[`cmd/internal/gc/reflect.go`](https://golang.org/src/cmd/compile/internal/gc/reflect.go)というファイルのコードと一致させる必要があるようです。

```go
// A header for a Go map.
type hmap struct {
	// Note: the format of the Hmap is encoded in ../../cmd/internal/gc/reflect.go and
	// ../reflect/type.go. Don't change this structure without also changing that code!
...
}
```


次の焦点は`cmd/internal/gc`付近になりそうです。

### hmapの作成はhmapという関数で

１つコメントの通りに[reflect.go](https://golang.org/src/cmd/compile/internal/gc/reflect.go)を覗いてみましょう。runtime/hashmap.goのhmapの構造体に対応するようなhmap()関数が定義されています。
単純に言えば、型のような情報を持った値を構文を走査(walk)している最中に作るという感じです。

(間に合わなかったことに対する恨みつらみをコメントに追加しています)

```go
// hmap builds a type representing a Hmap structure for the given map type.
// Make sure this stays in sync with ../../../../runtime/hashmap.go.
func hmap(t *types.Type) *types.Type {
    // 再帰的な走査の場合にこれがないと無限再帰に陥る。(これを新しく自分で追加しようとした型に忘れてハマったりしていたのも間に合わなかった原因の一つ)
    // これを忘れたときの振る舞いは面白く、静かに無限にメモリーを食べていきます
	if t.MapType().Hmap != nil {
		return t.MapType().Hmap
	}

    // 型定義に対応したフィールド設定(本当はもう少したくさんある)

	// build a struct:
	// type hmap struct {
...
	//    hash0      uint32
	//    buckets    *bmap
	//    oldbuckets *bmap  // oldとbucketsがあるのはサイズを伸張中でのイテレーションみたいなときに古い方もほしいので
...
	// }
	bmap := bmap(t)
	fields := []*types.Field{
...
		makefield("hash0", types.Types[TUINT32]), // Used in walk.go for OMAKEMAP.
		makefield("buckets", types.NewPtr(bmap)), // Used in walk.go for OMAKEMAP.
		makefield("oldbuckets", types.NewPtr(bmap)),
...
	}

	hmap := types.New(TSTRUCT)
	hmap.SetNoalg(true)
	hmap.SetFields(fields)
	dowidth(hmap)

    // ここに各種チェックがあったりする


    // この辺の記述を間違えると比較(cmp)のときにバグる
	t.MapType().Hmap = hmap
	hmap.StructType().Map = t
	return hmap
}
```

### 構文をparseして対応した方のOMAKEMAPでhmapを生成

ところでこのreflect.goで使われるhmapなどはどこで使われるのかというと。`gc/walk.go`のwalkexpr()という以下にもな名前の関数の中での分岐に使われます。各ノードに対応した処理が埋め込まれます。型情報が手に入るのでわーいヤッターという感じでhashmap.goでは確保するメモリのサイズが決められるようになります。


```go
// The result of walkexpr MUST be assigned back to n, e.g.
// 	n.Left = walkexpr(n.Left, init)
func walkexpr(n *Node, init *Nodes) *Node {

// nodeの持つ操作で分岐
	switch n.Op {
...
	case OMAKEMAP:
		t := n.Type
		hmapType := hmap(t)
		hint := n.Left
        .. 型チェックとか色々やる
...
    }
}
```

### イテレーションの始まりは `ORANGE`

イテレーションもどこかにあるはずで、walk.goからwalkrange()が呼ばれます。

```go
func walkstmt(n *Node) *Node {
	if n == nil {
		return n
	}

	switch n.Op {
..
	case ORANGE:
		n = walkrange(n)
	}
}
```

walkrange()は同じパッケージの[range.go](https://golang.org/src/cmd/compile/internal/gc/range.go)にあります。 walkrange()の中ではやっぱり型によって分岐が発生し良い感じのコードに変換されます(気持ちだけを伝えたいためにすごく省略しているので本来のコードが知りたい場合はリンク先のコードを見てみてください)。

```go
func walkrange(n *Node) *Node {

	t := n.Type
...

	switch t.Etype {
...
	case TMAP: // mapの時!!

.. //ぐちゃぐちゃとやっているけれど実質はこういう構文に変換したいということ
   for mapiterinit(t, h, hiter); hiter.key != nil; mapiternext(hiter) {
   }
...
}
```

イテレーションの方はなんとなくわかりましたね。

### 高速化のための特殊化も実は結構単純な方法で分岐

そして特殊化の話についてようやく触れます。時々至るところに現れた `_faststr` とか `_fast32` みたいなsuffixの話です。これも結局のところwalk.goの`walkexpr()`の付近での出来事です。

```go
func walkexpr(n *Node, init *Nodes) *Node {

	switch n.Op {
...
    case OINDEXMAP: // こういうのがある
        // Replace m[k] with *map{access1,assign}(maptype, m, &k)
		map_ := n.Left
		t := map_.Type

        // 本当は色々分岐がある
		fast := mapfast(t)
    	n = mkcall1(mapfn(mapassign[fast], t), nil, init, typename(t), map_, key)
    }
}
```

ここの部分実は読むのがちょっと大変なのですが。mapfnが呼び出す関数の分岐を行っています。

```go
func mapfn(name string, t *types.Type) *Node {
	if !t.IsMap() {
		Fatalf("mapfn %v", t)
	}
	fn := syslook(name)
	fn = substArgTypes(fn, t.Key(), t.Val(), t.Key(), t.Val())
	return fn
}
```

まず`syslook()`は`Runtimepkg.Lookup(name)`になり、結局のところruntimeに定義されているメンバーを関数名で検索します。
(つまり、mapfnに渡された時点で`mapaccess1`が`mapaccess1_fast32`であったりするということです)

ここでの分岐は単純でそのままenum 的なものを定義して型ディスパッチし、型から関数名を生成しているだけです(ランタイム時には動的な言語で結構やったりしますね)。

```go
const (
	mapslow = iota
	mapfast32
	mapfast64
	mapfaststr
	nmapfast
)

type mapnames [nmapfast]string

func mkmapnames(base string) mapnames {
	return mapnames{base, base + "_fast32", base + "_fast64", base + "_faststr"}
}

var mapaccess1 = mkmapnames("mapaccess1")
var mapaccess2 = mkmapnames("mapaccess2")
var mapassign = mkmapnames("mapassign")
var mapdelete = mkmapnames("mapdelete")

func mapfast(t *types.Type) int {
	// Check ../../runtime/hashmap.go:maxValueSize before changing.
	if t.Val().Width > 128 {
		return mapslow
	}
	switch algtype(t.Key()) {
	case AMEM32:
		return mapfast32
	case AMEM64:
		return mapfast64
	case ASTRING:
		return mapfaststr
	}
	return mapslow
}
```

## おわりに

本当はgoの魔改造を間に合わせたかったのですが。間に合わなかったのでその過程で得た情報を記事にすることにしました。
ちなみになぜ魔改造をしようと思うかというのはただただ楽しそうと思ったからであるし。
役に立つとかそういような不要な感情はお仕事をやるときだけで十分だという気持ちの現れでもあります。

goはgo自身で書かれているしオープンソースなのでgo自体のソースコードをいじるのも遊びとしては結構良いです。

追伸

冒頭で出てきたgo-swaggerについて、OAS3.0(現在はOAS2.0)対応のついでに内部でmapを使わずやるみたいな話が出ているようです。

- https://github.com/go-swagger/go-swagger/issues/1122

そして実用上は生成されたコードはPull Request上のdiffから除外できるのであんまり困って無いです。
