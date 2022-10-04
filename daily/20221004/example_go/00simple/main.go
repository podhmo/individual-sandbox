package main

import (
	"fmt"
	"go/token"
	"log"

	"github.com/podhmo/reflect-shape/metadata"
)

// Hello generates greeting message
func Hello(name string) string {
	return "Hello " + name
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
func run() error {
	l := metadata.NewLookup(token.NewFileSet())
	{
		fn := Hello
		metadata, err := l.LookupFromFunc(fn)
		if err != nil {
			return err
		}
		show(metadata)

	}
	{
		fn := fmt.Printf
		metadata, err := l.LookupFromFunc(fn)
		if err != nil {
			return err
		}
		show(metadata)

	}
	{
		ob := metadata.Lookup{}
		metadata, err := l.LookupFromStruct(ob)
		if err != nil {
			return err
		}
		fmt.Println("@", metadata.Raw.Name)
		fmt.Println("@", metadata.Raw.Doc)
		fmt.Println("@", metadata.Raw.FieldNames)
	}

	{
		ob := Person{}
		metadata, err := l.LookupFromStruct(ob)
		if err != nil {
			return err
		}
		fmt.Println("@", metadata.Raw.Name)
		fmt.Println("@", metadata.Raw.Doc)
		fmt.Println("@", metadata.Raw.FieldNames)
	}
	return nil
}

// Person is person object
type Person struct {
	Name string
}

func show(metadata *metadata.Func) {
	fmt.Println("\n\n")
	fmt.Printf("name is %q\n", metadata.Name())
	fmt.Printf("doc is %q\n", metadata.Doc())
	fmt.Printf("args is %v\n", metadata.Args())
	fmt.Printf("returns is %v\n", metadata.Returns())
	fmt.Println("----------------------------------------")
	fmt.Println(metadata.Fullname())

}
