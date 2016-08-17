package main

import (
	"fmt"
)

type Person struct {
    Name string
    Age int
}

func main(){
    person := Person{Name: "foo", Age: 20}
    // convenient output
    fmt.Printf("%%T %T\n", person)
    fmt.Printf("%%v %v\n", person)
    fmt.Printf("%%v %#v\n", person)

    // index access
    fmt.Printf("type=%[1]T, value=%[1]v, verbose=%#[1]v\n", person)

    // quoted string
    fmt.Printf("string = %q\n", "foo")

    // padding 0
    fmt.Printf("long=%06d, short=%04[1]d\n", 100)
}
