package main

import "fmt"
import "bytes"
import "encoding/json"
import "reflect"

// Checker :
type Checker struct {
	ID    func(ob interface{}) string
	cache map[interface{}]string
}

// Map :
func (c *Checker) Map(ob interface{}) string {
	rv := reflect.ValueOf(ob)
	rk := reflect.Indirect(rv).Interface()

	if v, ok := c.cache[rk]; ok {
		return v
	}
	v := c.ID(ob)
	c.cache[rk] = v
	return v
}

// Size :
func (c *Checker) Size() int {
	return len(c.cache)
}

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

	c := &Checker{ID: func(ob interface{}) string {
		var buf bytes.Buffer
		encoder := json.NewEncoder(&buf)
		if err := encoder.Encode(ob); err != nil {
			panic(err) // xxx
		}
		return buf.String()
	}, cache: map[interface{}]string{}}

	fmt.Println(c.Map(p1))
	fmt.Println(c.Map(p1))
	fmt.Println(c.Map(&p1))
	fmt.Println(c.Map(&p1))
	fmt.Println(c.Map(p2))
	fmt.Println(c.Map(p2))
	fmt.Println(c.Map(&p2))
	fmt.Println(c.Map(&p2))
	fmt.Println(c.Size())
}
