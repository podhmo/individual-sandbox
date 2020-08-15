package main

import (
	"bytes"
	"encoding/json"
	"io"
	"log"
	"os"
	"strconv"
	"sync"
)

var (
	pool = &sync.Pool{
		New: func() interface{} {
			var b bytes.Buffer
			return &b
		},
	}
)

type Person struct {
	Name   string  `json:"name"`
	Age    int     `json:"age"`
	Father *Person `json:"father,omitempty"`
	Mother *Person `json:"mother,omitempty"`
}

func (p *Person) MarshalJSON() ([]byte, error) {
	if p == nil {
		return []byte("null"), nil
	}
	// todo: grow
	buf := make([]byte, 64)
	i := 0
	buf[i] = '{'
	i++
	i += copy(buf[i:], `"name":`)
	buf[i] = '"'
	i++
	i += copy(buf[i:], p.Name)
	buf[i] = '"'
	i++
	buf[i] = ','
	i++

	i += copy(buf[i:], `"age":`)
	buf[i] = '"'
	i++
	i += copy(buf[i:], strconv.Itoa(p.Age))
	buf[i] = '"'
	i++
	buf[i] = ','
	i++

	if p.Father != nil {
		i += copy(buf[i:], `"father":`)
		sb, err := p.Father.MarshalJSON()
		if err != nil {
			return nil, err
		}
		i += copy(buf[i:], sb)
		buf[i] = ','
		i++
	}

	if p.Mother != nil {
		i += copy(buf[i:], `"mother":`)
		sb, err := p.Mother.MarshalJSON()
		if err != nil {
			return nil, err
		}
		i += copy(buf[i:], sb)
		buf[i] = ','
		i++
	}

	buf[i-1] = '}'
	return buf[:i], nil
}

func main() {
	if err := run(os.Stdout); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run(o io.Writer) error {
	p := &Person{
		Name: "foo",
		Age:  20,
		Father: &Person{
			Name: "bar",
			Age:  40,
		},
	}
	encoder := json.NewEncoder(o)
	encoder.SetIndent("", "  ")
	return runInner(p, encoder)
}

func runInner(p *Person, encoder *json.Encoder) error {
	return encoder.Encode(p)
}
