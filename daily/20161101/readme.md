# golang もしかして関数の中でstructは定義できるけれどメソッドは定義できない?

そういえば関数内で関数をトップレベルの関数と同じ記法で記述できなかった記憶がある。
以下の様な感じで迂回して記述する必要がありそう。

```go
// Greeter :
type Greeter interface {
	Greet() string
}

// SimpleGreeter :
type SimpleGreeter struct {
	greet func() string
}

// Greet :
func (sg *SimpleGreeter) Greet() string {
	return sg.greet()
}

func main() {
	hello := func() string {
		return "hello"
	}
	h := &SimpleGreeter{greet: hello}
	fmt.Println(h.Greet())
}
```

