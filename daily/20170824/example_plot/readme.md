当たり前のことではあるけれど

- `heapInUsed` は期待通りに増減する
- runtime.GC後に減少する

- pprofでheapはしっかり取れる
- pprofとheapの関連がはっきりしない

## memoryのこと

以下でやると良い(vsize,rssは共にKbyte)。

```
$ ps -o pid,vsize,rss,command
26779 556620768   4664 /var/folders/x7/s4rqw5z90r5bf9gd0l_yqztjsj_6zv/T/go-build119567627/command-line-arguments/_obj/exe/main
```

vsize,rssは共にkbyte。macのactivity monitorに表示されるものとは異なる。

|name|description|
|:--|:--|
|メモリ|13.6Mb|
|メモリ圧縮|43.6Mb|
|実メモリサイズ|4.6Mb (rss)|
|仮想メモリサイズ|530.83Gb (vss,vsize)|
|プライベートメモリサイズ|1.0Mb (uss)|

macで見つけるのむずい

- http://qiita.com/white_aspara25/items/cfc835006ae356189df3
- https://apple.stackexchange.com/questions/104/whats-the-difference-between-real-virtual-shared-and-private-memory
USS(unique set size), PSS(proportional set size), RSS(resident set size), VSS(virtual set size)
