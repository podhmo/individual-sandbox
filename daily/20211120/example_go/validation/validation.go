package validation

import (
	"fmt"
	"log"
	"m/validation/vm"
	"reflect"
	"strings"
)

type Validation func(*Context)
type HasValidation interface {
	Validation() []FieldValidation
}
type Context struct {
	*vm.CommandEmitter

	History *History
}

func New() *Context {
	return &Context{
		History:        &History{},
		CommandEmitter: &vm.CommandEmitter{},
	}
}

type History struct {
	Head Frame
	Tail *History
}

func (h *History) String() string {
	if h == nil {
		return ""
	}
	var path []string

	var walk func(*History)
	walk = func(x *History) {
		if x.Tail != nil {
			walk(x.Tail)
			path = append(path, strings.ToUpper(string(x.Head.Type))+":"+x.Head.Name)
		}
	}
	walk(h)
	return fmt.Sprintf("%v", path)
}

type FrameType string

const (
	FrameTypeRoot   FrameType = ""
	FrameTypeStruct FrameType = "struct"
	FrameTypeField  FrameType = "field"
	FrameTypeOption FrameType = "option"
)

type Frame struct {
	Type FrameType
	Name string
	RT   reflect.Type
}

func (c *Context) Current() Frame {
	return c.History.Head
}
func (c *Context) PushFrame(typ FrameType, target string, rt reflect.Type) {
	c.History = &History{Head: Frame{Name: target, Type: typ, RT: rt}, Tail: c.History}
}
func (c *Context) PopFrame() {
	c.History = c.History.Tail
}
func (c *Context) Path() string {
	return c.History.String()
}

func (c *Context) Visit(ob interface{}) {
	rt := reflect.TypeOf(ob)
	t, ok := ob.(HasValidation)
	if !ok {
		log.Printf("%v does not implement Validation()", rt)
		return
	}

	c.PushFrame(FrameTypeStruct, rt.String(), rt)
	c.EmitStruct(rt)
	defer c.PopFrame()

	fields := t.Validation()
	for _, f := range fields {
		f.Validation(c)
	}

	// TODO: auto attach struct-validation for nested-structure
}

type FieldValidation struct {
	Validation
	Name string
}

func Field(name string, options ...ValidationOption) FieldValidation {
	fn := func(c *Context) {
		rt := c.History.Head.RT
		if rt.Kind() == reflect.Ptr {
			rt = rt.Elem()
		}

		rf, ok := rt.FieldByName(name)
		if !ok {
			log.Printf("WARNING: field %q is not found. (in %s)", name, c.Path())
			return
		}

		c.PushFrame(FrameTypeField, name, rf.Type)
		defer c.PopFrame()
		c.EmitField(rt, rf)

		// handling nested structure (struct)
		// TODO: slice, array, map
		rft := rf.Type
		lv := 0
		if rft.Kind() == reflect.Ptr {
			rft = rft.Elem()
			lv++
		}
		if rft.Kind() == reflect.Struct {
			c.PushFrame(FrameTypeOption, "struct", nil)
			c.EmitOption("struct", lv)
			c.PopFrame()
		}

		for _, opt := range options {
			c.PushFrame(FrameTypeOption, opt.Name, nil)
			opt.Validation(c)
			c.PopFrame()
		}
	}
	return FieldValidation{
		Name:       name,
		Validation: fn,
	}
}

type ValidationOption struct {
	Validation
	Name        string
	Description string
}

func Required() ValidationOption {
	return ValidationOption{
		Name: "required",
		Validation: func(c *Context) {
			c.EmitOption("required")
		},
	}
}
