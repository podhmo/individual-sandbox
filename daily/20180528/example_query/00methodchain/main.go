package main

import (
	"errors"
	"log"

	"github.com/k0kubun/pp"
)

// User :
type User struct {
	Name string
	Age  int
}

// Q :
type Q map[string]interface{}

// UserQuery :
type UserQuery struct {
	q Q
}

// NewUserQuery :
func NewUserQuery() *UserQuery {
	return &UserQuery{q: Q{}}
}

// WithName :
func (u *UserQuery) WithName(name string) *UserQuery {
	q := Q{}
	for k, v := range u.q {
		q[k] = v
	}
	q["name"] = name
	return &UserQuery{q: q}
}

// WithAge :
func (u *UserQuery) WithAge(age int) *UserQuery {
	q := Q{}
	for k, v := range u.q {
		q[k] = v
	}
	q["age"] = age
	return &UserQuery{q: q}
}

// Do :
func (u *UserQuery) Do() (*User, error) {
	return nil, errors.New("404")
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	uq := NewUserQuery().WithName("alice").WithAge(20)
	pp.Println(uq)
	pp.Println(uq.Do())
	return nil
}

