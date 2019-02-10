package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
)

var (
	// ErrNotFound :
	ErrNotFound = fmt.Errorf("not found")
)

// Person :
type Person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

// PersonResponse :
type PersonResponse struct {
	Name     string `json:"name"`
	Age      int    `json:"age"`
	Nickname string `json:"nickname"`
	IconURL  string `json:"iconURL"`
}

// Fixer :
type Fixer interface {
	FixPerson(src *Person, dst *PersonResponse) error
}

// Converter :
type Converter struct {
	Fixer Fixer
}

// ConvertPerson :
func (c *Converter) ConvertPerson(src *Person) (*PersonResponse, error) {
	if src == nil {
		return nil, ErrNotFound
	}
	dst := &PersonResponse{
		Name: src.Name,
		Age:  src.Age,
	}
	if err := c.Fixer.FixPerson(src, dst); err != nil {
		return nil, err
	}
	return dst, nil
}

type fixer struct {
	DefaultAvatorURL string
}

// FixPerson :
func (f *fixer) FixPerson(src *Person, dst *PersonResponse) error {
	if len(src.Name) > 0 {
		dst.Nickname = string(src.Name[0])
	}
	dst.IconURL = f.DefaultAvatorURL
	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	p := &Person{Name: "foo", Age: 20}
	c := &Converter{Fixer: &fixer{DefaultAvatorURL: "https://examples.net/image/404.png"}}
	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	pr, err := c.ConvertPerson(p)
	if err != nil {
		return err
	}
	return encoder.Encode(pr)
}
