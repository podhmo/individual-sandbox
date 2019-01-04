package main

import "fmt"

type p struct {
	Name string
}

func main() {
	{
		xs := []p{{Name: "foo"}}
		ys := make([]p, len(xs))
		for i, x := range xs {
			x.Name = "bar"
			ys[i] = x
		}
		fmt.Println(xs, "->", ys)
	}
	{
		xs := []p{{Name: "foo"}}
		ys := make([]p, len(xs))
		for i, x := range xs {
			x := x
			x.Name = "bar"
			ys[i] = x
		}
		fmt.Println(xs, "->", ys)
	}
	{
		xs := []p{{Name: "foo"}}
		ys := make([]p, len(xs))
		for i := range xs {
			xs[i].Name = "bar"
			ys[i] = xs[i]
		}
		fmt.Println(xs, "->", ys)
	}
}
