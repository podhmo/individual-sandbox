# golangのequalityとmapのzero value

なるほど。

```go
type Point struct {
    x,y int
}

m := map[Point]int{}
m[Point{X: 10, Y: 20}]++
m[Point{X: 10, Y: 20}]++
m[Point{X: 10, Y: 10}]++
fmt.Printf("%v\n", m) // => map[{10 20}:2 {10 10}:1]
```
# golangの練習

golangの練習にgrepのsubsetでも作ろう

1. 正規表現を使う
2. コマンドライン引数で複数ファイル指定
3. コマンドライン引数でオプションを取れるようにする
4. ディレクトリ階層を再帰的な探索をする(`-r`)
5. grepの対象を絞る(`--include`, `--exclude`)
6. defaultのoptionを設定ファイルにより設定できる(tomlを使う)
7. 並行動作させる

