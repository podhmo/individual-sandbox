## python asyncio

- asyncioでcloseみたいな作業？ -> context manager
- read,writeをbytes単位で
- transport,protocolとstream -> 結局各自がobjectを持つというのが多くない？
- protocol -> event handling (strategy,hooks), 

web socketをprocessのように扱いたい場合の方法ということ X | Y | Z のproxy

## python aiohttp

- aiohttpでlog見れる？
- loop.add_readerってなんだろ？

- https://github.com/aio-libs/aiohttp/blob/master/examples/client_ws.py

## go run のときに引数に.goのファイルを渡す方法はどうすれば良いんだろう？

無理そうに見える

```go
func runRun(cmd *base.Command, args []string) {
	work.BuildInit()
	var b work.Builder
	b.Init()
	b.Print = printStderr
	i := 0
	for i < len(args) && strings.HasSuffix(args[i], ".go") {
		i++
	}
	var p *load.Package
	if i > 0 {
		files := args[:i]
		for _, file := range files {
			if strings.HasSuffix(file, "_test.go") {
				// GoFilesPackage is going to assign this to TestGoFiles.
				// Reject since it won't be part of the build.
				base.Fatalf("go run: cannot run *_test.go files (%s)", file)
			}
		}
		p = load.GoFilesPackage(files)
	} else if len(args) > 0 && !strings.HasPrefix(args[0], "-") {
		pkgs := load.PackagesAndErrors(args[:1])
		if len(pkgs) > 1 {
			var names []string
			for _, p := range pkgs {
				names = append(names, p.ImportPath)
			}
			base.Fatalf("go run: pattern %s matches multiple packages:\n\t%s", args[0], strings.Join(names, "\n\t"))
		}
		p = pkgs[0]
		i++
	} else {
		base.Fatalf("go run: no go files listed")
	}
	cmdArgs := args[i:]

..
}
```

### 追記

packageを指定できる。`./`で相対パスを強制するとどうにかなりそう。

```coonsole
$ go run ./<target dir> xxx.go
```

- packageを指定する
- `./`を忘れず付ける
