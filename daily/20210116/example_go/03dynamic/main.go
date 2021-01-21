package main

import (
	"fmt"
	"reflect"
	"strings"
)

type WalkOption func(*Walker)

func WithBreakdowns(breakdowns ...string) WalkOption {
	return func(w *Walker) {
		w.breakdowns = breakdowns
	}
}

func WithDynamicEnum(name string, fn func(p interface{}) []string) WalkOption {
	return func(w *Walker) {
		w.dynamicEnumFuncMap[name] = fn
	}
}

type Config struct {
	NameTag  string
	IndexTag string
	EnumTag  string

	MissingValue string
}

var DefaultConfig = Config{
	NameTag:  "json",
	IndexTag: "index",
	EnumTag:  "enum",

	MissingValue: "ANY",
}

type Walker struct {
	Config Config

	p interface{}

	fn func(parts []string)
	rt reflect.Type
	rv reflect.Value

	breakdowns         []string
	dynamicEnumFuncMap map[string]func(p interface{}) []string
}

var CheckErr = fmt.Errorf("check error")

func (w *Walker) Check() error {
	fieldnames := make([]string, w.rt.NumField())
	for i := 0; i < w.rt.NumField(); i++ {
		rf := w.rt.Field(i)
		fieldnames[i] = fieldName(rf, w.Config.NameTag)
	}

	for _, name := range w.breakdowns {
		found := false
		for _, fieldname := range fieldnames {
			if name == fieldname {
				found = true
				break
			}
		}
		if !found {
			return fmt.Errorf("%w -- breakdown %q is not found", CheckErr, name)
		}

		rf, _ := w.rt.FieldByName(name)
		if rf.Tag.Get(w.Config.IndexTag) == "dynamic-enum" {
			if _, ok := w.dynamicEnumFuncMap[name]; !ok {
				return fmt.Errorf("%w -- enum function for %q is not registered", CheckErr, name)
			}
		}
	}
	return nil
}

func (w *Walker) Walk(parts []string, i, n int) {
	if i >= n {
		w.fn(parts)
		return
	}

	rf := w.rt.Field(i)
	if rf.Tag.Get(w.Config.NameTag) == "-" {
		w.Walk(parts, i+1, n)
		return
	}

	name := fieldName(rf, w.Config.NameTag)
	value := w.rv.Field(i)

	found := false
	for _, k := range w.breakdowns {
		if name == k {
			found = true
			break
		}
	}
	if !found {
		v := w.Config.MissingValue
		if !value.IsZero() {
			v = fmt.Sprintf("%v", value.Interface())
		}
		w.Walk(append(parts, name, v), i+1, n)
		return
	}

	switch rf.Tag.Get(w.Config.IndexTag) {
	case "enum":
		for _, v := range strings.Split(rf.Tag.Get(w.Config.EnumTag), ",") {
			v = strings.TrimSpace(v)
			w.Walk(append(parts, name, v), i+1, n)
		}
	case "dynamic-enum":
		for _, v := range w.dynamicEnumFuncMap[name](w.p) {
			v = strings.TrimSpace(v)
			w.Walk(append(parts, name, v), i+1, n)
		}
	default: // literal
		w.Walk(append(parts, name, w.Config.MissingValue), i+1, n)
	}
}

func Walk(
	p interface{},
	fn func(parts []string),
	options ...WalkOption,
) {
	var parts []string
	w := &Walker{
		Config: DefaultConfig,

		p:  p,
		fn: fn,
		rv: reflect.ValueOf(p),
		rt: reflect.TypeOf(p),

		breakdowns:         nil,
		dynamicEnumFuncMap: map[string]func(p interface{}) []string{},
	}
	for _, opt := range options {
		opt(w)
	}
	if err := w.Check(); err != nil {
		panic(err) // xxx
	}
	w.Walk(parts, 0, w.rt.NumField())
}

func fieldName(rf reflect.StructField, tag string) string {
	name, ok := rf.Tag.Lookup(tag)
	if ok {
		return name
	}
	return rf.Name
}

type P struct {
	ID    string `json:"id" index:"literal"`
	Grade string `json:"grade" index:"enum" enum:"A,B,C,D,E"`
	Type  string `json:"type" index:"dynamic-enum"`
}

func main() {
	{
		fmt.Println("----------------------------------------")
		p := P{ID: "XXX"}
		Walk(p, func(parts []string) {
			fmt.Printf("%#+v	%q\n", p, strings.Join(parts, "/"))
		})
	}
	{
		fmt.Println("----------------------------------------")
		p := P{ID: "XXX", Grade: "C"}
		Walk(p, func(parts []string) {
			fmt.Printf("%#+v	%q\n", p, strings.Join(parts, "/"))
		})
	}
	{
		fmt.Println("----------------------------------------")
		p := P{ID: "XXX", Grade: "C"}
		Walk(p, func(parts []string) {
			fmt.Printf("%#+v	%q\n", p, strings.Join(parts, "/"))
		},
			WithBreakdowns("grade"),
		)
	}
	{
		fmt.Println("----------------------------------------")
		p := P{ID: "XXX", Grade: "C"}
		Walk(p, func(parts []string) {
			fmt.Printf("%#+v	%q\n", p, strings.Join(parts, "/"))
		},
			WithBreakdowns("type"),
			WithDynamicEnum("type", func(p interface{}) []string {
				return []string{"x", "y", "z"}
			}),
		)
	}
	{
		fmt.Println("----------------------------------------")
		p := P{ID: "XXX", Grade: "C"}
		Walk(p, func(parts []string) {
			fmt.Printf("%#+v	%q\n", p, strings.Join(parts, "/"))
		},
			WithBreakdowns("type", "grade"),
			WithDynamicEnum("type", func(p interface{}) []string {
				return []string{"x", "y", "z"}
			}),
		)
	}
}
