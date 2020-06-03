package main

import (
	"fmt"
	"reflect"
	"strings"

	"github.com/k0kubun/pp"
)

type Table string

type Field interface {
	Name() string
}
type StringField string

func (f StringField) Name() string {
	return string(f)
}

type People struct {
	Table Table
	Name  StringField
	XXX   string
}

func main() {
	people := People{
		Table: "people",
		Name:  StringField("name"),
		XXX:   "xxx",
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

	for i := 0; i < rdst.NumField(); i++ {
		df := rdst.Field(i)

		if df.CanSet() && df.Kind() != reflect.String {
			continue
		}

		ftype := df.Type()
		if ftype.AssignableTo(_rTable) {
			df.SetString(fmt.Sprintf("%s as %s", rsrc.Field(i).String(), prefix))
		} else if ftype.Implements(_rField) {
			df.SetString(prefix + "." + rsrc.Field(i).String())
		}
	}
}

var (
	_rTable = reflect.TypeOf((*Table)(nil)).Elem()
	_rField = reflect.TypeOf((*Field)(nil)).Elem()
)
