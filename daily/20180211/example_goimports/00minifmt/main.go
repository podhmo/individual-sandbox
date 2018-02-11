package main

import (
	"fmt"
	"log"

	"golang.org/x/tools/imports"
)

func main() {
	filename := "./user.go"
	input := `
package p
type User struct {Name string; Age int;}
`
	opt := &imports.Options{Comments: true, TabIndent: true, TabWidth: 8}
	output, err := imports.Process(filename, []byte(input), opt)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(string(output))
}
