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

type Node struct {
	Account Account
}

type Maskable interface {
	Mask() interface{}
}

func (u User) Mask() interface{} {
	u.Password = "***masked***"
	return u
}

func (n Node) Mask() interface{} {
	// panic
	n.Account = n.Account.Mask().(Account)
	return n
}

func main() {
	user := User{Name: "foo", Password: ":this is secret password:"}
	account := Account{User: user, IsActive: true}

	{
		var target Maskable = &Node{Account: account}
		fmt.Println("----------------------------------------")
		fmt.Printf("\tbefore: %#v\n", target)
		// we need `mapped: main.Account{main.User{Name:"foo", Password:"***masked***"} IsActive:true}` but..
		fmt.Printf("\tmapped: %#v\n", target.Mask())
		fmt.Printf("\tafter : %#v\n", target)
	}
}
