# golang goでの多値の扱い勘違いしていた(とくに引数)

複数引数取る関数に直接多値を返す関数を渡せる

```go
package main

import "fmt"

// NPlus :
func NPlus(n int) (int, int) {
	return n, n + 1
}

// Add :
func Add(x, y int) int {
	return x + y
}

func main() {
	fmt.Println(Add(NPlus(10)))
}
```

