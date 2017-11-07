package main

import "fmt"

// P :
type P struct {
	n int
	i *int
}

// NewP :
func NewP(n int) *P {
	i := 0
	return &P{i: &i, n: n}
}

// Inc :
func (p *P) Inc() {
	*p.i++
}

func main() {
	p := NewP(10)
	p.Inc()
	p.Inc()
	fmt.Println(p.n, *p.i)
}
