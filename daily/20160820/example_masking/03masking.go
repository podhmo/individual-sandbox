package main

import (
	"encoding/json"
	"fmt"
	"log"
	"regexp"
)

type User struct {
	Name     string
	Password string
}

type Maskable interface {
	Mask() interface{}
}
type I interface {
}

func Mask(o I) I {
	switch o := o.(type) {
	case Maskable:
		return o.Mask()
	default:
		return o
	}
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
