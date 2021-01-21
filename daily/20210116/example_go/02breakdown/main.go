package main

import (
	"fmt"
	"reflect"
	"strings"
)

type P struct {
	ID    string `json:"id" index:"literal"`
	Grade string `json:"grade" index:"enum" enum:"A,B,C,D,E"`
	Type  string `json:"type" index:"enum" enum:"x, y, z"`
}

func Resolve(p P) string {
	var parts []string
	rv := reflect.ValueOf(p)
	rt := reflect.TypeOf(p)
	for i := 0; i < rt.NumField(); i++ {
		rf := rt.Field(i)
		if IsSkipped(rf) {
			continue
		}
		v := rv.Field(i)
		if v.IsZero() {
			parts = append(parts, GetName(rf), "ANY")
		} else {
			parts = append(parts, GetName(rf), fmt.Sprintf("%v", v.Interface()))
		}
	}
	return strings.Join(parts, "/")
}

func Walk(
	p interface{},
	breakdowns []string,
	fn func(parts []string),
) {
	var parts []string
	w := &walker{
		p:          p,
		breakdowns: breakdowns,
		fn:         fn,
		rv:         reflect.ValueOf(p),
		rt:         reflect.TypeOf(p),
	}
	w.Walk(parts, 0, w.rt.NumField())
}

type walker struct {
	p  interface{}
	fn func(parts []string)
	rt reflect.Type
	rv reflect.Value

	breakdowns []string
}

func (w *walker) Walk(parts []string, i, n int) {
	if i >= n {
		w.fn(parts)
		return
	}

	rf := w.rt.Field(i)
	if IsSkipped(rf) {
		w.Walk(parts, i+1, n)
		return
	}

	name := GetName(rf)
	value := w.rv.Field(i)
	found := false
	for _, k := range w.breakdowns {
		if name == k {
			found = true
			break
		}
	}

	if !found {
		v := "ANY"
		if !value.IsZero() {
			v = fmt.Sprintf("%v", value.Interface())
		}
		w.Walk(append(parts, name, v), i+1, n)
		return
	}

	switch rf.Tag.Get("index") {
	case "enum":
		for _, v := range strings.Split(rf.Tag.Get("enum"), ",") {
			v = strings.TrimSpace(v)
			w.Walk(append(parts, name, v), i+1, n)
		}
	default: // literal
		w.Walk(append(parts, name, "ANY"), i+1, n)
	}
}

func IsSkipped(rf reflect.StructField) bool {
	return rf.Tag.Get("json") == "-"
}

func GetName(rf reflect.StructField) string {
	v, ok := rf.Tag.Lookup("json")
	if ok {
		return v
	}
	return rf.Name
}

func main() {
	{
		fmt.Println("----------------------------------------")
		p := P{ID: "XXX"}
		Walk(p, nil, func(parts []string) {
			fmt.Printf("%#+v	%q\n", p, strings.Join(parts, "/"))
		})
	}
	{
		fmt.Println("----------------------------------------")
		p := P{ID: "XXX", Grade: "C"}
		Walk(p, nil, func(parts []string) {
			fmt.Printf("%#+v	%q\n", p, strings.Join(parts, "/"))
		})
	}
	{
		fmt.Println("----------------------------------------")
		p := P{ID: "XXX", Grade: "C"}
		Walk(p, []string{"grade"}, func(parts []string) {
			fmt.Printf("%#+v	%q\n", p, strings.Join(parts, "/"))
		})
	}
	{
		fmt.Println("----------------------------------------")
		p := P{ID: "XXX", Grade: "C"}
		Walk(p, []string{"type"}, func(parts []string) {
			fmt.Printf("%#+v	%q\n", p, strings.Join(parts, "/"))
		})
	}
	{
		fmt.Println("----------------------------------------")
		p := P{ID: "XXX", Grade: "C"}
		Walk(p, []string{"type", "grade"}, func(parts []string) {
			fmt.Printf("%#+v	%q\n", p, strings.Join(parts, "/"))
		})
	}
}
