package s

import "fmt"

type S struct {
	Children []*S
}

func (s *S) M() {
	fmt.Println("hello")
	s.m()
	for _, c := range s.Children {
		c.cm()
	}
}
func (s *S) m() {
	fmt.Println("hello")
	f()
}
func (s *S) cm() {
	fmt.Println("hello")
	f()
}
func G() {
	fmt.Println("hello", len("hello"))
	f()
}
func F() {
	fmt.Println("hello", len("hello"))
	f()
	G()
}
func f() {
	fmt.Println("hello")
}
func Rec(n int) {
	if n < 0 {
		return
	}
	Rec(n - 1)
}
func Even(n int) bool {
	if n <= 0 {
		return true
	}
	return Odd(n - 1)
}
func Odd(n int) bool {
	if n == 1 {
		return false
	}
	return Even(n - 1)
}
