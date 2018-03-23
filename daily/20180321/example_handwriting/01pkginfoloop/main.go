package main

import (
	"fmt"
	"log"

	"golang.org/x/tools/go/loader"
)

func main() {
	c := loader.Config{
		TypeCheckFuncBodies: func(path string) bool { return false },
	}

	c.Import("bytes")

	prog, err := c.Load()
	if err != nil {
		log.Fatal(err)
	}

	info := prog.Package("bytes")
	{
		fmt.Println("----------------------------------------")
		fmt.Println("Defs")
		for k, v := range info.Defs {
			fmt.Println(k, v)
		}
		fmt.Println("----------------------------------------")
	}

	{
		fmt.Println("----------------------------------------")
		fmt.Println("Types")
		for k, v := range info.Types {
			fmt.Println(k, v)
		}
		fmt.Println("----------------------------------------")
	}
}
