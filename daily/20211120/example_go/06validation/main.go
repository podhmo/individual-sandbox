package main

import (
	"context"
	"fmt"
	. "m/06validation/x"
)

func main() {
	p := Person{Father: &Person{}, Children: []Person{{}, {Name: "Foo"}, {}}}
	v := &Validator{Translator: &Translator{Lang: "ja"}}
	ctx := context.Background()

	fmt.Println(v.Validate(ctx, &p))
	fmt.Println("----------------------------------------")
	fmt.Println(v.Validate(ctx, p))
	fmt.Println("----------------------------------------")
	fmt.Println(v.Validate(ctx, []Person{p, p}))
	fmt.Println("----------------------------------------")
	fmt.Println(v.Validate(ctx, []*Person{&p, nil, &p}))
	fmt.Println("----------------------------------------")
	fmt.Println(v.Validate(ctx, [][]*Person{{&p, nil, &p}, {&p, nil, &p}}))
	fmt.Println("----------------------------------------")
	fmt.Println(v.Validate(ctx, [][]Person{{p, p}, {p, p}}))
	fmt.Println("----------------------------------------")
	fmt.Println(v.Validate(ctx, map[string]Person{"x": p, "y": p}))
}
