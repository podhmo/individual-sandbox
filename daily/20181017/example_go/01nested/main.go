package main

import "fmt"
import "reflect"

// Person :
type Person struct {
	Name   string  `json:"name"`
	Age    int     `json:"age"`
	Father *Person `json:"father"`
	Mother *Person `json:"mother"`
}

func main() {
	p1 := Person{Name: "foo", Age: 20}
	p2 := Person{Name: "foo", Age: 20}
	p3 := Person{Name: "foo", Age: 20, Father: &p1}
	p4 := Person{Name: "foo", Age: 20, Father: &p1, Mother: &p1}
	p5 := Person{Name: "foo", Age: 20, Father: &p2, Mother: &p1}

	fmt.Println("shallow equal", p1 == p2)
	fmt.Println("shallow equal", &p1 == &p2)

	fmt.Println("deep equal", reflect.DeepEqual(p1, p2))
	fmt.Println("deep equal", reflect.DeepEqual(&p1, &p2))
	fmt.Println("deep equal", reflect.DeepEqual(&p1, &p3))
	fmt.Println("deep equal", reflect.DeepEqual(&p1, &p4))
	fmt.Println("deep equal", reflect.DeepEqual(&p1, &p5))

	fmt.Println("----------------------------------------")
	fmt.Println("deep equal", reflect.DeepEqual(&p3, &p4))
	fmt.Println("deep equal", reflect.DeepEqual(&p4, &p5))
}
