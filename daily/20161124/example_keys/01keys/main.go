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
	Y    *string
	Type KeyType
}

func f() []keys {
	bar := "bar"
	boo := "boo"
	return []keys{
		{X: "foo", Y: &bar, Type: KeyTypeA},
		{X: "foo", Y: &bar, Type: KeyTypeB},
		{X: "foo", Y: &boo, Type: KeyTypeA},
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
    // map[{foo 0xc42006e030 a}:1 {foo 0xc42006e050 a}:1 {foo 0xc42006e050 b}:1 {foo 0xc42006e070 a}:1 {foo 0xc42006e030 b}:1 {foo 0xc42006e040 a}:1 {foo 0xc42006e060 a}:1 {foo 0xc42006e070 b}:1 {foo 0xc42006e080 a}:1]
}
