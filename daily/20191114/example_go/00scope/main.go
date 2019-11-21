package main

import (
	"errors"
	"fmt"
)

// User ...
type User struct {
	Name string
}

func main() {
	ok := true
	fmt.Printf("result %+v\n", use(ok))
}

func use(ok bool) *User {
	var user *User
	with(func() {
		user, err := find(ok)
		if err != nil {
			panic(err)
		}
		fmt.Printf("found: %+v\n", user)
	})
	return user
}

func with(fn func()) {
	fn()
}

func find(ok bool) (*User, error) {
	var user User
	if ok {
		user.Name = "ok"
		return &user, nil
	}
	return nil, errors.New("not found")
}
