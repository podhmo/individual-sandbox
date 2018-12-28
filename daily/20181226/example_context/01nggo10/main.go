package main

import (
	"context"
	"fmt"

	xxx "./xxx"
	yyy "./yyy"
)

func main() {
	ctx := yyy.WithContext(xxx.WithContext(context.Background()))
	fmt.Println("get from xxx", xxx.GetValue(ctx))
	fmt.Println("get from yyy", yyy.GetValue(ctx))
}
