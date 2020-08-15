package main

import (
	"bytes"
	"encoding/json"
	"io"
	"log"
	"os"
	"strconv"
)

type Person struct {
	Name   string  `json:"name"`
	Age    int     `json:"age"`
	Father *Person `json:"father,omitempty"`
	Mother *Person `json:"mother,omitempty"`
}

func (p *Person) MarshalJSON() ([]byte, error) {
	var b bytes.Buffer // これを使い回せないんだろうか？
	if p == nil {
		return []byte("null"), nil
	}
	b.WriteByte('{')

	b.WriteString(`"name":`)
	b.WriteByte('"')
	b.WriteString(p.Name)
	b.WriteByte('"')
	b.WriteByte(',')

	b.WriteString(`"age":`)
	b.WriteByte('"')
	b.WriteString(strconv.Itoa(p.Age))
	b.WriteByte('"') // todo: use strconv.AppendInt
	b.WriteByte(',')

	if p.Father != nil {
		b.WriteString(`"father":`)
		sb, err := p.Father.MarshalJSON()
		if err != nil {
			return nil, err
		}
		b.Write(sb)
		b.WriteByte(',')
	}

	if p.Mother != nil {
		b.WriteString(`"mother":`)
		sb, err := p.Father.MarshalJSON()
		if err != nil {
			return nil, err
		}
		b.Write(sb)
		b.WriteByte(',')
	}
	b.Truncate(b.Len() - 1)
	b.WriteByte('}')
	return b.Bytes(), nil
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
