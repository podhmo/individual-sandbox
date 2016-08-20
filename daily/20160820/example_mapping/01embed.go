package main

import (
	"fmt"
)

type User struct {
	Name     string
	Password string
}

type Account struct {
	User
	IsActive bool
}

type Maskable interface {
	Mask() interface{}
}

func (u User) Mask() interface{} {
	u.Password = "***masked***"
	return u
}

func main() {
	user := User{Name: "foo", Password: ":this is secret password:"}
	account := Account{User: user, IsActive: true}

    {
        var target Maskable = user
        fmt.Println("----------------------------------------")
        fmt.Printf("\tbefore: %#v\n", target)
        fmt.Printf("\tmapped: %#v\n", target.Mask())
        fmt.Printf("\tafter : %#v\n", target)
    }
    {
        var target Maskable = account
        fmt.Println("----------------------------------------")
        fmt.Printf("\tbefore: %#v\n", target)
        // we need `mapped: main.Account{main.User{Name:"foo", Password:"***masked***"} IsActive:true}` but..
        fmt.Printf("\tmapped: %#v\n", target.Mask())
        fmt.Printf("\tafter : %#v\n", target)
    }
}
