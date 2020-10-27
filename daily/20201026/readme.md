## go コードリーディングを支える技術

- 呼び出し関係の可視化
- caller

### 追記

まずはgopackageあたりでディレクトリを調べたい。
最も軽いのは0?

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

        // NeedModule adds Module.
        NeedModule
)
```

ファイルの一覧がほしい場合はこんな感じで良さそう。

```
	cfg := &packages.Config{
		Mode: packages.NeedName,
	}
	pkgs, err := packages.Load(cfg, targets...)
```

### 裏側で実行されているgo listの引数を見たいとき

```
packages.Config{
	Logf: log.Printf
}
```

`GOPACKAGESDEBUG=1` の効果がわからない。あー、タイポしてた。gopackages-debug。

## go いい感じにastを辿る方法

- File.Scope.Objectsはやっぱり駄目かも。methodが取れない。
- go/docを覗いてみよう。ast.NewPackage()って何をするんだっけ？

  - scopeを作ってimporterを呼びまくってるな。
  - unresolvedなobjectも探している
  - あとouterにuniverseをセットしている

- go/packagesも覗いてみるとParseFileを変更できるのか

## reflect-openapi

- ciの追加
- configにcontextをもたせるのはありだなー。 (from go/packages)
