package main

import (
	"fmt"
	"log"
)

type Person struct {
	Name string
}

func Load(ob interface{}) error {
	xs := []Person{{Name: "foo"}}
	ptr, _ := ob.(*[]Person)
	*ptr = xs
	return nil
}

func main() {
	var obs []Person
	if err := Load(&obs); err != nil {
		log.Fatalf("!! %+v", err)
	}
	fmt.Println(obs)
}
