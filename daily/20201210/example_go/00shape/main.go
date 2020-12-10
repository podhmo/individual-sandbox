package main

import (
	"encoding/json"
	"fmt"

	"github.com/podhmo/reflect-openapi/pkg/shape"
)

func Get(d map[string]string, k string) string {
	return d[k]
}

func main() {
	{
		s := shape.Extract(Get)
		b, err := json.MarshalIndent(s, "", "  ")
		fmt.Printf("%s\n%+v\n", b, err)
		fmt.Println("----------------------------------------")
	}
}
