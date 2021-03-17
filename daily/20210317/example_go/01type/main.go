package main

import (
	"fmt"
	"io"
	"m/def"
	"os"
)

func main() {
	c := def.DefaultConfig

	helloPkg := c.NewPackage("hello", "m/hello")
	defaultName := helloPkg.NewVariable(
		"DefaultName", nil,
		"Foo",
	)
	hello := helloPkg.NewFunction(
		"Hello", nil,
		func(env def.Env, w io.Writer) error {
			_, err := io.WriteString(w, `func Hello(name string) string {
	return fmt.Sprintf("Hello %s", name)l
}`)
			return err
		},
	)

	fmt.Println(helloPkg)
	fmt.Println(hello)
	fmt.Println(defaultName)
	fmt.Println("----------------------------------------")
	helloPkg.Describe(os.Stdout)
}
