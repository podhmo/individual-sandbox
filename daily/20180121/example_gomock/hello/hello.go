package hello

// HelloWorld :
func HelloWorld(h Hello, needCall bool) {
	if needCall {
		h.Hello("hello world")
	}
}

// Hello :
type Hello interface {
	Hello(s string)
}

type hello struct {
	fn func(string)
}

func (h *hello) Hello(s string) {
	h.fn(s)
}
