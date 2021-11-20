package main

import (
	"context"
	"fmt"
	. "m/06validation/x"
)

func main() {
	p := Person{Father: &Person{}}
	v := &Validator{Translator: &Translator{Lang: "ja"}}
	ctx := context.Background()
	fmt.Println(v.Validate(ctx, &p))
}
