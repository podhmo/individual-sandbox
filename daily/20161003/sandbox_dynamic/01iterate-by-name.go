package main

import (
	"log"
	"reflect"
)

type person struct {
	FirstName string
	LastName  string
	Age       int
}

func walkByNames(o interface{}, names []string) (map[string]string, error) {
	m := make(map[string]string)
	v := reflect.ValueOf(o)
	for _, s := range names {
		field := v.FieldByName(s)
		if field.IsValid() {
			m[s] = field.String()
		}
	}
	return m, nil
}

func main() {
	p := person{FirstName: "foo", LastName: "bar", Age: 20}
	m, err := walkByNames(p, []string{"FirstName", "LastName"})
	if err != nil {
		log.Fatal(err)
	}
	log.Println(m)
}
