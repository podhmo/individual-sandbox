## golang x/tools/go/loaderを尊重したinterfaceの何か

尊重した何かを作りたい。
違いは全ての依存を一気に読むのをやめる点。
それでも似たような形にした方がわかりやすいだろうということはある気がする。

- CreateFromFiles
- Import

この２つの違いは何かということだよなー。それはpackage単位かそうでないかという話。あるいは作られたpackageがimport可能かそうでないか。
それぞれの機能を書き出してみる。

### Config

Configが色々なめんどくさい機能の省略。facadeっぽい感じ。基本的な使いかたは以下。

```go
c := &Config{}
c.Import("<package name>")
c.Import("<package name>")
prog, err := c.Load()
```

`Load()` をするまでに読み込むべきパッケージを指定しておく。`Load()`を利用したら用済み。

そして、実際のところはパッケージ単位よりも細かい粒度で指定したくなることがある。特に現在のWorkingArea(GOPATH以下)に存在しないようなファイルsを対象としたい場合など。そんなときには自分の手で未だ存在しないパッケージを作り上げる必要がある。そんな時に使うのは `CreateFromFiles()`, `CreateFromFilenames()`。それぞれの違いは実際のファイルが存在するか。存在しないか。

具体的には以下のようなstructが登録されていく。FilesかFilenamesを持つ。

```
type PkgSpec struct {
	Path      string      // package path ("" => use package declaration)
	Files     []*ast.File // ASTs of already-parsed files
	Filenames []string    // names of files to be parsed
}
```


このConfigが覆い隠したいpackageは以下のもの。

- go/token
- go/parser

覆い隠したいだけあって、例えば新たにちょっとしたコード片を追加したいときにも、Config経由でASTを読み込めるようになっている(実際にはgo/parserのwrapper)。

```
f, err := c.ParseFile("<file name>", "<source>")
```

### Program

Programはloaderが読み込んだ結果。価値観的には読み込んだものへのアクセスを手軽にするようなAccessor的な機能。あるいは単にパッケージの置き場と考えても良いかもしれない。

名前で直接取得できる。取れるのは `*types.Package` 自体ではなくそれをwrapした `PackageInfo`

```go
pkginfo, err := prog.Package("<package name>")
```

別の見方をすると、これは、Configで指定してLoadした結果。当然ではあるけれど。`Import()`されたものや`CreateFromFiles()`されたものを開始地点のパッケージとして取り出したくなる。それには `InitialPackages()` というメソッドが用意されている。

```go
pkgs := prog.InitialPackages()
```

内部的には、さらにCreatedとImportedに分かれる。

## x/toolsとの違いは何だろう？

作りたいものとx/toolsのものとの違いは何だろう？ポイントは幾つかある。ただまとめてしまうと。x/tools/go/loaderの方は全ての工程が一回きりになっているという点があるかもしれない。例えば依存パッケージのimportを`Load()`したときに全部やってしまう。

これは仕方がない面もあって、loadする、すなわち型情報などの値(types.Package)を読み込むということが主体なので、その型チェックのタイミングでは全ての情報がひと揃いにあるという状態を仮定したほうがやりやすい。それに結局整合性を調べるためには全てが必要になる。という前提があるので。

一方で何に困っているかと言うと。大きなパッケージの一部分を取り出そうとした時にそこそこの負荷になってしまう点。より正確に言えば、ファイル上の不要なimportの部分まで全部loadされてしまう点。違いはここでimportの解決を遅延させたい。ただしそのように考えると、Configは原因でProgramは結果というような単純な構造で認識することはできなくなる。


