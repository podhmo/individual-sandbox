## go AST

goでAST触るのにめんどくさい部分はこの辺りだよなー。前者だとGenDecl.Commentに入り後者だとTypeSpec.Doc

```go
// Post ...
type Post struct {
	Id      int64 `db:"post_id"`
	Created int64
	Title   string `db:",size:50"`               // Column size set to 50
	Body    string `db:"article_body,size:1024"` // Set both column name and size
}

type (
	// Post2 ...
	Post2 struct {
		Id      int64 `db:"post_id"`
		Created int64
		Title   string `db:",size:50"`               // Column size set to 50
		Body    string `db:"article_body,size:1024"` // Set both column name and size
	}
)
```


## go gopackage

- NeedNameだけだと依存パッケージを読まない
- NeedName + NeedImportsだとfakeな感じで全部読む
- NeedName + NeedFilesだと

```console
# NeedName
53.417029ms for GOROOT= GOPATH=./example_go go [go list -e -json -compiled=false -test=false -export=false -deps=false -find=true -- ./database]

# NeedName + NeedImports
163.82699ms for GOROOT= GOPATH=./example_go go [go list -e -json -compiled=false -test=false -export=false -deps=true -find=false -- ./database]
13.94838ms for GOROOT= GOPATH=./example_go go [go env -json GOMOD GOPATH]

# NeedName + NeedTypes
284.816911ms for GOROOT= GOPATH=./example_go go [go list -e -json -compiled=true -test=false -export=true -deps=false -find=false -- ./database]
15.668762ms for GOROOT= GOPATH=./example_go go [go env -json GOMOD GOPATH]

# NeedName + NeedFiles
52.88283ms for GOROOT= GOPATH=./example_go go [go list -e -json -compiled=false -test=false -export=false -deps=false -find=true -- ./database]
```

定期的に忘れる

```
const (
	// NeedName adds Name and PkgPath.
	NeedName LoadMode = 1 << iota

	// NeedFiles adds GoFiles and OtherFiles.
	NeedFiles

	// NeedCompiledGoFiles adds CompiledGoFiles.
	NeedCompiledGoFiles

	// NeedImports adds Imports. If NeedDeps is not set, the Imports field will contain
	// "placeholder" Packages with only the ID set.
	NeedImports

	// NeedDeps adds the fields requested by the LoadMode in the packages in Imports.
	NeedDeps

	// NeedExportsFile adds ExportFile.
	NeedExportsFile

	// NeedTypes adds Types, Fset, and IllTyped.
	NeedTypes

	// NeedSyntax adds Syntax.
	NeedSyntax

	// NeedTypesInfo adds TypesInfo.
	NeedTypesInfo

	// NeedTypesSizes adds TypesSizes.
	NeedTypesSizes

	// typecheckCgo enables full support for type checking cgo. Requires Go 1.15+.
	// Modifies CompiledGoFiles and Types, and has no effect on its own.
	typecheckCgo

	// NeedModule adds Module.
	NeedModule
)
```

## go packagetest

便利な世の中になったものだ。

- https://pkg.go.dev/golang.org/x/tools/go/packages/packagestest?tab=doc

## python fastapi

あとなにかあったっけ？

- https://github.com/tiangolo/full-stack-fastapi-postgresql
- https://github.com/nsidnev/fastapi-realworld-example-app
