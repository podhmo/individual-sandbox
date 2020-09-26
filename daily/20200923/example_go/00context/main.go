package main

import (
	"context"
	"fmt"
)

type K string

const (
	kx = K("x")
	ky = K("y")
)

func main() {
	ctx := context.Background()
	ctx = context.WithValue(ctx, kx, 1)
	fmt.Println(ctx.Value(kx))
	ctx = context.WithValue(ctx, ky, 10)
	ctx = context.WithValue(ctx, kx, 100)
	fmt.Println(ctx.Value(kx))

	// 	pp.Println(ctx)

	// 1
	// 100
	// &context.valueCtx{
	//   Context: &context.valueCtx{
	//     Context: &context.valueCtx{
	//       Context: &0,
	//       key:     "x",
	//       val:     1,
	//     },
	//     key: "y",
	//     val: 10,
	//   },
	//   key: "x",
	//   val: 100,
	// }
}
