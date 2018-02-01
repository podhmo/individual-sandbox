## go go/importerの効用

簡単にpkgが得られる。
一気にpkgで取れる。

importerの取り出し方

```
importer.Default() // 通常 "gc"
```

sourceを直接

```
importer.For("source")
```

## go mvpkgの使い方

```
$ go get -v golang.org/x/tools/cmd/gomvpkg
$ gomvpkg -from github.com/podhmo/something/xyz -to github.com/podhmo/something/service/xyz
```
