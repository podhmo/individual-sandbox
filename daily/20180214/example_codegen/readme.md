## golang何かに使えそう

```go
	loaded := map[string]token.Pos{}
	fset.Iterate(func(f *token.File) bool {
		if strings.HasPrefix(f.Name(), c.Build.GOPATH) {
			loaded[f.Name()[len(c.Build.GOPATH):]] = token.Pos(f.Base())
		}
		return true
	})
```
