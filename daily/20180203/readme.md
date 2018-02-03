## go gorenameを早くする

はじめの段階

```
Renamed 105 occurrences in 43 files in 22 packages.

real    0m28.323s
user    0m53.647s
sys     0m21.475s
```

### importgraphのpathを制限する

以下でダメ

```
could not import golang.org/x/text/secure/bidirule (invalid package name: "")
```

これはgo/typesのresolver.goのエラー

```
		if err == nil && imp != nil && (imp.name == "_" || imp.name == "") {
			err = fmt.Errorf("invalid package name: %q", imp.name)
			imp = nil // create fake package below
		}
```

resolver自体はfake packageを作っている。

golang.org/x/tools/go/loaderのappendErrorでエラー扱いになっているっぽい？

golang.org/x/tools/go/loader/のnewPackageInfoで生成されるchecker

```
func (imp *importer) newPackageInfo(path, dir string) *PackageInfo {
	tc.Importer = closure{imp, info}
	tc.Error = info.appendError // appendError wraps the user's Error function

	info.checker = types.NewChecker(&tc, imp.conf.fset(), pkg, &info.Info)
	imp.progMu.Lock()
	imp.prog.AllPackages[pkg] = info
	imp.progMu.Unlock()
	return info
}

type closure struct {
	imp  *importer
	info *PackageInfo
}

func (c closure) Import(to string) (*types.Package, error) { return c.imp.doImport(c.info, to) }
```
