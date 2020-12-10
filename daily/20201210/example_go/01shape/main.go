package main

import (
	"encoding/json"
	"fmt"

	"github.com/podhmo/reflect-openapi/pkg/shape"
)

type Getter interface {
	Get(k string) string
}

func main() {
	{
		s := shape.Extract(func() *Getter { return nil }())
		b, err := json.MarshalIndent(s, "", "  ")
		fmt.Printf("%s\n%+v\n", b, err)
		fmt.Printf("%+v\n", s)
		fmt.Println("----------------------------------------")
	}
}
