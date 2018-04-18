## golang go-tools

この辺見てみる気になった。

- https://github.com/dominikh/go-tools

どの辺りを見るのが良いだろう？

とりあえず[lint](https://github.com/dominikh/go-tools/tree/master/lint)は見ておく?
とは言え全体でどういうふうに使われているかも後で見たいような気がする？

### lint

https://godoc.org/honnef.co/go/tools/lint

- "honnef.co/go/tools/ssa" のssaを理解しきれていない気がする
- IsXXX系の関数がいっぱい
- トップレベルで使う構造体はなんだろ？

:thought_balloon: たぶんSSA(Static Single Assignment form)

https://godoc.org/honnef.co/go/tools/ssa この辺に詳しく書いてある


使い方は `ssautil.CreateProgram(program loader.Program)` みたいなものを呼べば良いっぽい。

---

中を覗く

Linter。これはloader.Configを受け取るっぽい(xtoolsのloader)
とは言え、Load()は使わず、CreateFromFilesとParseFileしか使っていないな。
いや違う。LinterのLint()にloader.Programを渡している(その手前でLoad()してる)。
(このあたりはtestutil/util.go付近を読むと良さそう)

結構全部入りのものを lint.Pkg, lint.Programとして作っている(SSAしたものを入れているけれどどこで使っているかはまだ調べてない)。

:thought_balloon: そういえば、packageのgraphを作ったあとwalkしてくれるのは誰なんだろ？

`NodeFns()` が内部の globalVisitorを使ってvisitしているのか。ちなみにwalkの対象は普通にloaderから取得したpackageを使っていそう？

並列数は以下でとっている。

:thought_balloon: loaderのProgramのInitialPackagesって何者だっけ？(記憶が確かならCreateFromFilesで作られたpackage)

```
runtime.NumCPU()*2
```
