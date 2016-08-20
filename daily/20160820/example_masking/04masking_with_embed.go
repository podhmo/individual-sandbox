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

type DetailedUser struct {
	User
	Memo string
}

type Maskable interface {
	Mask() interface{}
}
type I interface {
}

func Mask(o I) I {
	if o, ok := o.(Maskable); ok {
		return o.Mask()
	}
	return o
}

func (u User) Mask() interface{} {
	// object u is copied.
	rx := regexp.MustCompile(".")
	u.Password = rx.ReplaceAllLiteralString(u.Password, "*")
	return u
}

func (u DetailedUser) Mask() interface{} {
    // hmm
    u.User = u.User.Mask().(User)
    return u
}

func main() {
	user := &User{Name: "foo", Password: "*this is secret*"}
	duser := &DetailedUser{User: *user, Memo: "hmm"}
	// masking user
	{
		data, err := json.Marshal(Mask(user))
		if err != nil {
			log.Fatal(err)
		}
		fmt.Printf("danger:\n\t%s\n", string(data))
	}
	// masking duser
	{
		data, err := json.Marshal(Mask(duser))
		if err != nil {
			log.Fatal(err)
		}
		fmt.Printf("danger:\n\t%s\n", string(data))
	}
}
