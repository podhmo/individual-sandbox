package main

import (
	"fmt"
	"log"

	"github.com/podhmo/validator/tagscan"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type Person struct {
	Name string `validate:"@foo"`
	Age  int
}

func run() error {
	{
		c := tagscan.NewConfigDefault()
		xs, err := c.Scanner().ScanAll(Person{})
		fmt.Printf("%+#v, %+v\n", xs, err)
		for _, x := range xs {
			fmt.Println(x.Describe())
		}
	}
	fmt.Println("----------------------------------------")
	{
		c := tagscan.NewConfigDefault()
		var p *Person
		xs, err := c.Scanner().ScanAll(p)
		fmt.Printf("%+#v, %+v\n", xs, err)
		for _, x := range xs {
			fmt.Println(x.Describe())
		}
	}
	return nil
}
