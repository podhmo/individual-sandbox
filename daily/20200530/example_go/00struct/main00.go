package main

import (
	"fmt"
	"reflect"
	"strings"

	"github.com/k0kubun/pp"
)

type Table string
type People struct {
	Table Table
	Name  string
}

func main() {
	people := People{
		Table: "people",
		Name:  "name",
	}
	p := people
	Alias(&p, &people, "p")
	pp.Println(people, p)
}

func Alias(dst, src interface{}, name string) {
	prefix := strings.TrimSuffix(name, ".")
	WithPrefix(dst, src, prefix)
}

func WithPrefix(dst, src interface{}, prefix string) {
	rsrc := reflect.ValueOf(src).Elem()
	rdst := reflect.ValueOf(dst).Elem()
	rtype := reflect.TypeOf(src).Elem()

	for i := 0; i < rtype.NumField(); i++ {
		df := rdst.Field(i)

		if df.CanSet() && df.Kind() != reflect.String {
			continue
		}
		if rtype.Field(i).Name == "Table" {
			df.SetString(fmt.Sprintf("%s as %s", rsrc.Field(i).String(), prefix))
		} else {
			df.SetString(prefix + "." + rsrc.Field(i).String())
		}
	}
}
