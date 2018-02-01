## go ファイルやディレクトリ作成するとき

```
directory 0755
file 0744
```

0忘れずに。

```
os.MkdirAll(<>, 0755)
```

## go gorenameを架空のFS上で行いたい

gorenameのテストだと以下のパッケージのFakeContextが使われている。

- golang.org/x/tools/go/buildutil

:warning: 殆どはうまくいくのだけれどrename後のwriteFileが実在するファイルを取り扱っていて上手く行かない

### buildutilのFakeContext

渡すコードは以下の様な感じ。

```go
	pkgs := map[string]map[string]string{
		"go/src/foo": map[string]string{
			"foo.go": `
package foo

type Foo struct{}
`,
		},
	}
	ctxt := buildutil.FakeContext(pkgs)
```

これでExistsなどが動く

```go
	{
		fmt.Println("exists ? ", "go/src/foo/foo.go")
		fmt.Println("	", buildutil.FileExists(ctxt, "go/src/foo/foo.go"))
		fmt.Println("exists ? ", "go/src/bar/bar.go")
		fmt.Println("	", buildutil.FileExists(ctxt, "go/src/bar/bar.go"))
	}
```

### あともうすこし


殆どはうまくいくのだけれどrename後のwriteFileが実在するファイルを取り扱っていて上手く行かない

具体的には以下の定義が使われる(diffを渡した場合で関数が変わる事はあるけれど。基本これ)

```go
// writeFile is a seam for testing and for the -d flag.
var writeFile = reallyWriteFile

func reallyWriteFile(filename string, content []byte) error {
	return ioutil.WriteFile(filename, content, 0644)
}
```

テストのときには自分自身のpackageなのでunexportedな変数にも触れる。

```go
		got := make(map[string]string)
		writeFile = func(filename string, content []byte) error {
			got[filepath.ToSlash(filename)] = string(content)
			return nil
		}
```

#### monkey

- https://github.com/bouk/monkey

これは公開されたものの実装の差し替えなので無理じゃん？

