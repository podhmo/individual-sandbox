package main

import (
	"fmt"
	"reflect"
	"strings"
)

type P struct {
	ID    string `json:"id"`
	Grade string `json:"grade"`
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
		p := P{ID: "xxx", Grade: "A"}
		fmt.Printf("%#+v	%q\n", p, Resolve(p))
	}
	{
		p := P{ID: "xxx", Grade: "B"}
		fmt.Printf("%#+v	%q\n", p, Resolve(p))
	}
	{
		p := P{ID: "xxx", Grade: "C"}
		fmt.Printf("%#+v	%q\n", p, Resolve(p))
	}
	{
		p := P{ID: "xxx"}
		fmt.Printf("%#+v	%q\n", p, Resolve(p))
	}
}
