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

func walk(p *person) (map[string]string, error) {
	m := make(map[string]string)
	v := reflect.ValueOf(*p)
	if v.FieldByName("FirstName").IsValid() {
		log.Printf("%[1]T: %#[1]v\n", v.FieldByName("FirstName").String())
		m["FirstName"] = v.FieldByName("FirstName").String()
	}
	if v.FieldByName("LastName").IsValid() {
		log.Printf("%[1]T: %#[1]v\n", v.FieldByName("LastName").String())
		m["LastName"] = v.FieldByName("LastName").String()
	}
	return m, nil
}

func main() {
	p := person{FirstName: "foo", LastName: "bar", Age: 20}
	m, err := walk(&p)
	if err != nil {
		log.Fatal(err)
	}
	log.Println(m)
}
