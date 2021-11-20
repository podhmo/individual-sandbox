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
	seen    map[reflect.Type]bool

	indirectBuf []indirectItem
}
type indirectItem struct {
	Type    reflect.Type
	Field   reflect.StructField
	Parent  reflect.Type
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
	{
		k := rt
		if rt.Kind() == reflect.Ptr {
			k = rt.Elem()
		}
		if c.seen == nil { // xxx
			c.seen = map[reflect.Type]bool{}
		}
		if _, ok := c.seen[k]; ok {
			return
		}
		c.seen[k] = true
	}

	c.PushFrame(FrameTypeStruct, rt.String(), rt)
	defer c.PopFrame()
	c.EmitStruct(rt)

	seen := map[string]bool{}
	t, ok := ob.(HasValidation)
	if !ok {
		log.Printf("%v does not implement Validation()", rt)
	} else {
		fields := t.Validation()
		for _, f := range fields {
			seen[f.Name] = true
			f.Validation(c)
		}
	}

	// auto attach struct-validation
	// TODO: slice, array, map
	if rt.Kind() == reflect.Ptr {
		rt = rt.Elem()
	}
	if rt.Kind() == reflect.Struct {
		for i := 0; i < rt.NumField(); i++ {
			rf := rt.Field(i)
			if _, ok := seen[rf.Name]; ok {
				continue
			}
			Field(rf.Name).Validation(c)
		}
	}
}

func (c *Context) VisitIndirect() {
	log.Println("visit indirect...")
	n := 20            // max depth
	if c.seen == nil { // xxx
		c.seen = map[reflect.Type]bool{}
	}

	for i := 0; i < n; i++ {
		buf := c.indirectBuf
		c.indirectBuf = nil

		for _, item := range buf {
			rt := item.Type
			k := rt
			if rt.Kind() == reflect.Ptr {
				k = rt.Elem()
			}
			if _, ok := c.seen[k]; ok {
				continue
			}
			c.seen[k] = true

			c.PushFrame(FrameTypeStruct, rt.String(), rt)
			defer c.PopFrame()
			s := c.EmitStruct(rt)

			if rt.Name() == "" { // hmm: find better anonymous struct's name
				s.Name = fmt.Sprintf("%s_%s", vm.StructOf(c.CommandEmitter, item.Parent).Name, item.Field.Name)
				s.Anonymous = true
			}

			// auto attach struct-validation
			// TODO: slice, array, map
			if rt.Kind() == reflect.Ptr {
				rt = rt.Elem()
			}
			if rt.Kind() == reflect.Struct {
				for i := 0; i < rt.NumField(); i++ {
					rf := rt.Field(i)
					// struct only?
					Field(rf.Name).Validation(c)
				}
			}
		}

		if c.indirectBuf == nil {
			return
		}
	}
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
			c.indirectBuf = append(c.indirectBuf, indirectItem{
				Type:    rft,
				Field:   rf,
				Parent:  rt,
				History: c.History,
			})
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
