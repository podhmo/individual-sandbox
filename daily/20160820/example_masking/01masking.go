package main

import (
	"encoding/json"
	"fmt"
	"log"
	"reflect"
	"regexp"
)

type User struct {
	Name     string
	Password string
}

type Maskable interface {
	Mask() interface{}
}

func Mask(o interface{}) interface{} {
	v := reflect.ValueOf(o)
	if t, ok := v.Interface().(Maskable); ok {
		return t.Mask()
	}
	return o
}

func (u User) Mask() interface{} {
	// object u is copied.
	rx := regexp.MustCompile(".")
	u.Password = rx.ReplaceAllLiteralString(u.Password, "*")
	return u
}

func main() {
	user := &User{Name: "foo", Password: "*this is secret*"}

	// when after POST(create) response...
	{
		data, err := json.Marshal(user)
		if err != nil {
			log.Fatal(err)
		}
		// password value is exposed..
		fmt.Printf("danger:\n\t%s\n", string(data))
	}

	// masking
	{
		data, err := json.Marshal(Mask(user))
		if err != nil {
			log.Fatal(err)
		}
		fmt.Printf("danger:\n\t%s\n", string(data))
	}
}
