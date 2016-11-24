package main

import "fmt"

// KeyType :
type KeyType string

// KeyType
const (
	KeyTypeA = KeyType("a")
	KeyTypeB = KeyType("b")
)

type keys struct {
	X    string
	Y    string
	Type KeyType
}

func f() []keys {
	return []keys{
		{X: "foo", Y: "bar", Type: KeyTypeA},
		{X: "foo", Y: "bar", Type: KeyTypeB},
		{X: "foo", Y: "boo", Type: KeyTypeA},
	}
}

func main() {
	m := map[keys]int{}
	for _, k := range f() {
		m[k]++
	}
	for _, k := range f() {
		m[k]++
	}
	for _, k := range f() {
		m[k]++
	}
	fmt.Println(m)
    // map[{foo boo a}:3 {foo bar a}:3 {foo bar b}:3]
}
