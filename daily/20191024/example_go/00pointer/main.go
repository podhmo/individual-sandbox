package main

import "github.com/k0kubun/pp"

type p struct {
	Key   string
	Value int
}

func main() {
	x := &p{Key: "x", Value: 0}
	y := &p{Key: "y", Value: 0}
	input := []*p{x, y}

	tmp := []*p{x}

	tmp[0].Value = 1
	pp.Println(input)
}

// []*main.p{
//   &main.p{
//     Key:   "x",
//     Value: 1,
//   },
//   &main.p{
//     Key:   "y",
//     Value: 0,
//   },
// }
