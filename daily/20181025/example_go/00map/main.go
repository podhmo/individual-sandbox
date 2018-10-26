package main

import "github.com/k0kubun/pp"

func main() {
	type p struct {
		Name string
	}

	{
		m := map[int]p{1: p{}}
		for k := range m {
			p := m[k]
			p.Name = "foo"
		}
		pp.Println(m)
	}
	{
		m := map[int]*p{1: &p{}}
		for k := range m {
			p := m[k]
			p.Name = "foo"
		}
		pp.Println(m)
	}

	type p2 struct {
		*p
	}
	{
		m := map[int]p2{1: p2{p: &p{}}}
		for k := range m {
			p := m[k]
			p.Name = "foo"
		}
		pp.Println(m)
	}
	{
		m := map[int]*p2{1: &p2{p: &p{}}}
		for k := range m {
			p := m[k]
			p.Name = "foo"
		}
		pp.Println(m)
	}

}
