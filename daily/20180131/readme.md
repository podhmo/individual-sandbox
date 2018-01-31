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
