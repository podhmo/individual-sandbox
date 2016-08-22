# golang graceful stop

参考

- [Go 1.7 Release Party in Tokyo -- context パッケージの紹介](http://go-talks.appspot.com/github.com/matope/talks/2016/context/context.slide#1)
- [http.Serverをgracefulに停止させる - Qiita](http://qiita.com/nyamage/items/35feebdb1d39a570930a)

# golang 同じ文字をN回使った文字列を生成

`strings.Repeat()` を使えば良い。

```
strings.Repeat(" ", 10)
```
