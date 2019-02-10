package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os"

	"github.com/pkg/errors"
)

var (
	// ErrNotFound :
	ErrNotFound = fmt.Errorf("not found")
)

// Group :
type Group struct {
	Name    string   `json:"name"`
	Members []Person `json:"members"`
}

// Person :
type Person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

// GroupNode :
type GroupNode struct {
	*Group
}

// PersonNode :
type PersonNode struct {
	Group *GroupNode
	*Person
}

// GroupView :
type GroupView struct {
	Name    string       `json:"name"`
	Members []PersonView `json:"members"`
}

// PersonView :
type PersonView struct {
	Name     string `json:"name"`
	Age      int    `json:"age"`
	Fullname string `json:"fullname"`
	Nickname string `json:"nickname"`
	IconURL  string `json:"iconURL"`
}

// Fixer :
type Fixer interface {
	// FixGroup(src *GroupNode, dst *GroupView) error
	FixPerson(src *PersonNode, dst *PersonView) error
}

// Converter :
type Converter struct {
	Fixer Fixer
}

// ConvertGroup :
func (c *Converter) ConvertGroup(src *Group) (*GroupView, error) {
	dst := &GroupView{}
	return dst, c.convertGroupNode(&GroupNode{src}, dst)
}

func (c *Converter) convertGroupNode(src *GroupNode, dst *GroupView) error {
	if src == nil {
		return errors.Wrap(ErrNotFound, "convert group")
	}
	dst.Name = src.Name

	// all fields are filled, so internal fix method is not needed
	// if err := c.FixGroup(src, dst); err != nil {
	// 	return nil, err
	// }

	dst.Members = make([]PersonView, len(src.Members))
	for i := range src.Members {
		csrc := src.Members[i]
		cdst := PersonView{}
		if err := c.convertPersonNode(&PersonNode{src, &csrc}, &cdst); err != nil {
			return errors.Wrap(err, fmt.Sprintf("members[%d]", i))
		}
		dst.Members[i] = cdst
	}
	return nil
}

// this is not toplevel object so. not declared
// // ConvertPerson :
// func (c *Converter) ConvertPerson(src *Person) (*PersonView, error) {
// 	return c.convertPersonNode(&PersonNode{src})
// }

func (c *Converter) convertPersonNode(src *PersonNode, dst *PersonView) error {
	if src == nil {
		return errors.Wrap(ErrNotFound, "convert person")
	}

	dst.Name = src.Name
	dst.Age = src.Age

	if err := c.Fixer.FixPerson(src, dst); err != nil {
		return err
	}
	return nil
}

type fixer struct {
	DefaultAvatorURL string
}

// FixPerson :
func (f *fixer) FixPerson(src *PersonNode, dst *PersonView) error {
	// calculate from other fields (computed property)
	var xNickname string
	if len(src.Name) > 0 {
		xNickname = string(src.Name[0])
	}

	// using parent information
	var xFullname string
	if src.Group != nil {
		xFullname = fmt.Sprintf("%s / %s", src.Group.Name, src.Name)
	}

	// using setting information
	xIconURL := f.DefaultAvatorURL

	return f.bindPerson(
		dst,
		nickname(xNickname),
		fullname(xFullname),
		iconURL(xIconURL),
	)
}

type (
	nickname string
	fullname string
	iconURL  string
)

func (f *fixer) bindPerson(
	dst *PersonView,
	nickname nickname,
	fullname fullname,
	iconURL iconURL,
) error {
	dst.Nickname = string(nickname)
	dst.Fullname = string(fullname)
	dst.IconURL = string(iconURL)
	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	p := &Person{Name: "foo", Age: 20}
	g := &Group{Name: "A", Members: []Person{*p}}
	c := &Converter{Fixer: &fixer{DefaultAvatorURL: "https://examples.net/image/404.png"}}

	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")

	pr, err := c.ConvertGroup(g)
	if err != nil {
		return err
	}
	return encoder.Encode(pr)
}
