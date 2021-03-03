package main

import (
	"fmt"
	"reflect"

	"github.com/jmoiron/sqlx/reflectx"
)

type Person struct {
	Name string `db:"name,unique"`
	Age  int64  `db:"age,default=20"`
}

func main() {
	mapper := reflectx.NewMapper("db")
	tmap := mapper.TypeMap(reflect.TypeOf(Person{}))
	for i, info := range tmap.Index {
		fmt.Printf("%d\t%s\ttags=%+#v\n", i, info.Name, info.Options)
	}
}
