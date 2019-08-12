package main

import (
	"reflect"

	"github.com/jmoiron/sqlx/reflectx"
	"github.com/k0kubun/pp"
)

type Foo struct {
	A string
	B int
	C int    `db:"-"`
	D string `db:"ddd"`
}

func main() {
	m := reflectx.NewMapperFunc("db", func(s string) string { return s })
	v := Foo{A: "foo", B: 10, C: 100}

	rv := reflect.ValueOf(v)
	rt := reflect.TypeOf(v)

	fields := m.TypeMap(rt)

	pp.Println("index of fields", len(fields.Index))
	pp.Println("field by name", m.FieldByName(rv, "A").Interface(), v.A)

	for i, f := range fields.Index {
		pp.Println(
			i, f.Name,
			m.FieldByName(rv, f.Name).Interface(),
			reflectx.FieldByIndexesReadOnly(rv, f.Index).Interface(),
		)
	}
}
