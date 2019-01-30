package main

import (
	"fmt"
	"log"

	"github.com/k0kubun/pp"
)

// Doc :
type Doc interface {
	ID() string
}

// Modify :
type Modify interface {
	Doc
	Modify()
}

type p struct {
	id   string
	name string
}

func (p *p) ID() string {
	return p.id
}

func (p *p) Modify() {
	p.name = fmt.Sprintf("**%s**", p.name)
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	var data []Modify
	for i := 0; i < 3; i++ {
		data = append(data, &p{id: fmt.Sprintf("%d", i), name: fmt.Sprintf("foo%d", i)})
	}

	docs := make([]Doc, len(data))
	for i := range data {
		data[i].Modify()
		docs[i] = data[i]
	}

	pp.Println(docs)
	return nil
}
