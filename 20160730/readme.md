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

https://gist.github.com/podhmo/3cd3c1cd8bb1392dafc2eedc07f3cf35
