package main

import "fmt"
import "bytes"
import "encoding/json"

// Checker :
type Checker struct {
	ID    func(ob interface{}) string
	cache map[interface{}]string
}

// Map :
func (c *Checker) Map(ob interface{}) string {
	// slice is unhashable
    // using https://github.com/mitchellh/hashstructure ?
    v := c.ID(ob)
	return v
}

// Size :
func (c *Checker) Size() int {
	return len(c.cache)
}

// Equal :
func (c *Checker) Equal(x, y interface{}) bool {
	return c.Map(x) == c.Map(y)
}

// Person :
type Person struct {
	Name   string  `json:"name"`
	Age    int     `json:"age"`
	Nums   []int   `json:"nums"`
	Father *Person `json:"father"`
	Mother *Person `json:"mother"`
}

// Person2 :
type Person2 struct {
	Name   string   `json:"name"`
	Age    int      `json:"age"`
	Nums   []int    `json:"nums"`
	Father *Person2 `json:"father"`
	Mother *Person2 `json:"mother"`
}

func main() {
	c := &Checker{ID: func(ob interface{}) string {
		var buf bytes.Buffer
		encoder := json.NewEncoder(&buf)
		if err := encoder.Encode(ob); err != nil {
			panic(err) // xxx
		}
		return buf.String()
	}, cache: map[interface{}]string{}}

	p1 := Person{Name: "foo", Age: 20, Nums: []int{1, 2, 3}}
	p2 := Person2{Name: "foo", Age: 20, Nums: []int{1, 2, 3}}
	p3 := Person{Name: "foo", Age: 20, Father: &p1}
	p4 := Person{Name: "foo", Age: 20, Father: &p1, Mother: &p1}
	p5 := Person2{Name: "foo", Age: 20, Father: &p2, Mother: &p2}

	fmt.Println("deep equal", c.Equal(p1, p2))
	fmt.Println("deep equal", c.Equal(&p1, &p2))
	fmt.Println("deep equal", c.Equal(&p1, &p3))
	fmt.Println("deep equal", c.Equal(&p1, &p4))
	fmt.Println("deep equal", c.Equal(&p1, &p5))

	fmt.Println("----------------------------------------")
	fmt.Println("deep equal", c.Equal(&p3, &p4))
	fmt.Println("deep equal", c.Equal(&p4, &p5))
}
