package main

import "fmt"
import "reflect"

// Person :
type Person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

func main() {
	p := Person{Name: "foo", Age: 20}
	p2 := Person{Name: "foo", Age: 20}

	fmt.Println("shallow equal", p == p2)
	fmt.Println("shallow equal", &p == &p2)
	fmt.Println("deep equal", reflect.DeepEqual(p, p2))
	fmt.Println("deep equal", reflect.DeepEqual(&p, &p2))
}
