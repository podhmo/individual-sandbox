package main

import "fmt"

func d(xs []*int) {
	ys := make([]int, len(xs))
	for i, x := range xs {
		ys[i] = *x
	}
	fmt.Println(ys)
}

func main() {
	xs := []int{}
	for i := 0; i < 5; i++ {
		xs = append(xs, i)
	}

	// かんりゃくかしたもの
	{
		ys := []*int{}
		for _, x := range xs {
			ys = append(ys, &x)
		}
		d(ys)
	}

	// かんりゃくかしたもの(その２)
	{
		gen := func(i *int) *int { return i }

		ys := []*int{}
		for _, x := range xs {
			x := gen(&x)
			ys = append(ys, x)
		}
		d(ys)
	}

	// じっさい
	{
		gen := func(i *int) (*int, error) { return i, nil }

		ys := []*int{}
		for _, x := range xs {
			x, err := gen(&x)
			if err != nil {
				panic(err)
			}
			ys = append(ys, x)
		}
		d(ys)
	}

	// せいかい
	{
		ys := []*int{}
		for _, x := range xs {
			x := x
			ys = append(ys, &x)
		}
		d(ys)
	}
	{
		ys := []*int{}
		for i := range xs {
			ys = append(ys, &xs[i])
		}
		d(ys)
	}
}

// [4 4 4 4 4]
// [4 4 4 4 4]
// [4 4 4 4 4]
// [0 1 2 3 4]
