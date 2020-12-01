package main

import "log"

type User struct {
	Name string
}

// domain
var users []User

func RegisterUser(user User) error {
	users = append(users, user)
	return nil
}

func run() error {
	user := User{
		Name: "foo",
	}
	if err := RegisterUser(user); err != nil {
		return err
	}
	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
