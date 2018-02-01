## go ? go/buildとgo/importerの関係

- build.Import
- importer.Default


## go gorenameをlibraryとして使う

golang.org/x/tools/refactor/rename

## go gorenameを使う

privateにしたい

- http://blog.ralch.com/tutorial/golang-tools-refactoring/

```
$ gorename -from '"github.com/podhmo/sandbox/model".Person' -to person
```
