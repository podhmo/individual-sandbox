## go package

- GOPACKAGESDRIVER=という環境変数でいじれるっぽい
- query自体はgolist.goに書かれている
- file=はDirを上手く扱わないとだめでむずかしい感じ

## go tool trace

1. runtime.trace.Start()
2. runtime.trace.Stop()
3. go tool trace <trace file>

## go flagsを手に馴染ませる

- init()
- usage()
- flags.Varを使えるものに対してのinit(flags.Value)

### flags.Varについて

特別なaccessor(flags.Value)を定義して使うもののよう。

```go
type Value interface {
	String() string
	Set(string) error
}

type Getter interface {
	Value
	Get() interface{}
}
```

そしてmainでは以下の様な感じ

```go
// flags
var (
	depsFlag  = flag.Bool("deps", false, "show dependencies too")
	testFlag  = flag.Bool("test", false, "include any tests implied by the patterns")
	mode      = flag.String("mode", "imports", "mode (one of files, imports, types, syntax, allsyntax)")
	private   = flag.Bool("private", false, "show non-exported declarations too")
	printJSON = flag.Bool("json", false, "print package in JSON form")

	cpuprofile = flag.String("cpuprofile", "", "write CPU profile to this file")
	memprofile = flag.String("memprofile", "", "write memory profile to this file")
	traceFlag  = flag.String("trace", "", "write trace log to this file")

	buildFlags stringListValue
)

func init() {
	flag.Var(&buildFlags, "buildflag", "pass argument to underlying build system (may be repeated)")
}

func usage() {
	fmt.Fprintln(os.Stderr, `Usage: gopackages [-deps] [-cgo] [-mode=...] [-private] package...

The gopackages command loads, parses, type-checks,
and prints one or more Go packages.

Packages are specified using the notation of "go list",
or other underlying build system.

Flags:`)
	flag.PrintDefaults()
}

func main() {
	log.SetPrefix("gopackages: ")
	log.SetFlags(0)
	flag.Usage = usage
	flag.Parse()

	if len(flag.Args()) == 0 {
		usage()
		os.Exit(1)
	}
}
```

なるほど。

## go profile (wrap run)

```go
	if *cpuprofile != "" {
		f, err := os.Create(*cpuprofile)
		if err != nil {
			log.Fatal(err)
		}
		if err := pprof.StartCPUProfile(f); err != nil {
			log.Fatal(err)
		}
		// NB: profile won't be written in case of error.
		defer pprof.StopCPUProfile()
	}

	if *traceFlag != "" {
		f, err := os.Create(*traceFlag)
		if err != nil {
			log.Fatal(err)
		}
		if err := trace.Start(f); err != nil {
			log.Fatal(err)
		}
		// NB: trace log won't be written in case of error.
		defer func() {
			trace.Stop()
			log.Printf("To view the trace, run:\n$ go tool trace view %s", *traceFlag)
		}()
	}

	if *memprofile != "" {
		f, err := os.Create(*memprofile)
		if err != nil {
			log.Fatal(err)
		}
		// NB: memprofile won't be written in case of error.
		defer func() {
			runtime.GC() // get up-to-date statistics
			if err := pprof.WriteHeapProfile(f); err != nil {
				log.Fatalf("Writing memory profile: %v", err)
			}
			f.Close()
		}()
	}


```

## go gopackagesコマンド

```
$ vgo get -v golang.org/x/tools/go/packages/gopackages
```

## go Config 的な何か(go/packagesを例に)

Configに設定をもたせてConfigの状態を使わない

- nilを初期値(undefined)として使える
- Load(cfg,..)のなかでloaderを作成してDriverをとりだしDriverResponseをparseしてpackages
- 雑にまとまりをpackageと呼んでいるところがあるな。

### go/packagesのmodeの扱い

- modeのlevelごとにcomponentを用意
- levelが不足しているときにはcomponentの参照をnilに
- 手軽に一度きりにしたい場合にはsync.Once
- syntaxのときにはneedsrcがtrueになり、Syntax(*ast.File)への参照
- allSyntaxとの違いはignoreFuncBody

## go x/tools/go/packages

https://godoc.org/golang.org/x/tools/go/packages

go/packagesの使いかたを調べてみる。

```
vgo get -v golang.org/x/tools/go/packages
```

利用できるqueryは？

```
file=path/to/file.go
pattern=string
name=identifier // will be available soon
```

一般的なコード

```go
	cfg := &packages.Config{Mode: packages.LoadSyntax}
	pkgs, err := packages.Load(cfg, flag.Args()...)
```

利用できるmodeは?files,imports,types,syntax,allSyntaxという順にマジで調べていくのかな？

```go
const (
	// LoadFiles finds the packages and computes their source file lists.
	// Package fields: ID, Name, Errors, GoFiles, and OtherFiles.
	LoadFiles LoadMode = iota

	// LoadImports adds import information for each package
	// and its dependencies.
	// Package fields added: Imports.
	LoadImports

	// LoadTypes adds type information for package-level
	// declarations in the packages matching the patterns.
	// Package fields added: Types, Fset, and IllTyped.
	// This mode uses type information provided by the build system when
	// possible, and may fill in the ExportFile field.
	LoadTypes

	// LoadSyntax adds typed syntax trees for the packages matching the patterns.
	// Package fields added: Syntax, and TypesInfo, for direct pattern matches only.
	LoadSyntax

	// LoadAllSyntax adds typed syntax trees for the packages matching the patterns
	// and all dependencies.
	// Package fields added: Types, Fset, Illtyped, Syntax, and TypesInfo,
	// for all packages in the import graph.
	LoadAllSyntax
)
```

### 上手く動かせないんだけれど？

- Config.Dir重要じゃない？emptyだとcwdになる
- Driverというのは何者？defaultはgoListDriver(file,pattern,name)のprefixで分割している

driver?

```go
type driver func(cfg *Config, patterns ...string) (*driverResponse, error)
```
