package main

import (
	"fmt"
	"reflect"
	"strings"
)

type P struct {
	ID    string `json:"id" index:"literal"`
	Grade string `json:"grade" index:"enum" enum:"A,B,C,D,E"`
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

func Iter(p P, fn func(parts []string)) {
	var parts []string
	rv := reflect.ValueOf(p)
	rt := reflect.TypeOf(p)
	iter(p, fn, rt, rv, parts, 0, rt.NumField())
}

func iter(
	p P,
	fn func(parts []string),
	rt reflect.Type,
	rv reflect.Value,
	parts []string,
	i, n int,
) {
	if i >= n {
		fn(parts)
		return
	}

	rf := rt.Field(i)
	if IsSkipped(rf) {
		iter(p, fn, rt, rv, parts, i+1, n)
		return
	}

	name := GetName(rf)
	value := rv.Field(i)
	if !value.IsZero() {
		s := fmt.Sprintf("%v", value.Interface())
		iter(p, fn, rt, rv, append(parts, name, s), i+1, n)
		return
	}

	switch rf.Tag.Get("index") {
	case "enum":
		for _, v := range strings.Split(rf.Tag.Get("enum"), ",") {
			v = strings.TrimSpace(v)
			iter(p, fn, rt, rv, append(parts, name, v), i+1, n)
		}
	default: // literal
		iter(p, fn, rt, rv, append(parts, name, "ANY"), i+1, n)
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
		p := P{ID: "XXX"}
		Iter(p, func(parts []string) {
			fmt.Printf("%#+v	%q\n", p, strings.Join(parts, "/"))
		})
	}
	{
		p := P{ID: "XXX", Grade: "C"}
		Iter(p, func(parts []string) {
			fmt.Printf("%#+v	%q\n", p, strings.Join(parts, "/"))
		})
	}
}
