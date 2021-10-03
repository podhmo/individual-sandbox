package main

import (
	"context"
	"fmt"
)

func main() {
	ctx := context.Background()
	k := "K"

	ctx = context.WithValue(ctx, k, 10)
	fmt.Println(ctx.Value(k))

	{
		ctx := context.WithValue(ctx, k, 20)
		fmt.Println(ctx.Value(k))
	}
	fmt.Println(ctx.Value(k))
}
