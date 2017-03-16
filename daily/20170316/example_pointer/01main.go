package main

import "fmt"

// P :
type P struct {
	KS []string
	X  *int
}

func main() {
	v := 1
	v2 := 10
	// v3 := 100
	ps := []*P{&P{X: &v, KS: []string{"b"}}, &P{X: &v, KS: []string{"a", "b"}}, &P{X: &v2, KS: []string{"b"}}}

	m := map[string]*P{}

	for _, p := range ps {
		for _, k := range p.KS {
			k := k
			q, ok := m[k]
			if !ok {
				newP := *p
				newP.X = p.X
				m[k] = &newP
			} else {
				*q.X += *p.X
			}
		}
	}
	fmt.Println(*ps[0].X)
	fmt.Println(*ps[1].X)
	fmt.Println(*ps[2].X)
	fmt.Println("----------------------------------------")
	fmt.Println(*m["a"].X)
	fmt.Println(*m["b"].X)
}
