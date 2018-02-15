## golang go/types.Info について

https://github.com/golang/example/tree/master/gotypes

あれこれ便利じゃない？

```
// Info holds result type information for a type-checked package.
// Only the information for which a map is provided is collected.
// If the package has type errors, the collected information may
// be incomplete.
type Info struct {
	Types map[ast.Expr]TypeAndValue
	Defs map[*ast.Ident]Object
	Uses map[*ast.Ident]Object
	Implicits map[ast.Node]Object
	Selections map[*ast.SelectorExpr]*Selection
	Scopes map[ast.Node]*Scope
	InitOrder []*Initializer
}
```

## golang pkgのload時にtype checkをskipするのに便利なもの

```go
	conf := &loader.Config{
		TypeCheckFuncBodies: func(path string) bool {
			return false // skip type check
		},
	}
```
