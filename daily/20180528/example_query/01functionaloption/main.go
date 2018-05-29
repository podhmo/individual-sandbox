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
func NewUserQuery(opts ...func(q *UserQuery)) *UserQuery {
	q := &UserQuery{q: Q{}}
	for _, opt := range opts {
		opt(q)
	}
	return q
}

// WithName :
func WithName(name string) func(*UserQuery) {
	return func(q *UserQuery) {
		q.q["name"] = name
	}
}

// WithAge :
func WithAge(age int) func(*UserQuery) {
	return func(q *UserQuery) {
		q.q["age"] = age
	}
}

// DoUserQuery :
func DoUserQuery(user *User, opts ...func(*UserQuery)) error {
	uq := NewUserQuery(opts...)
	_ = uq
	// bind(&user)
	return errors.New("404")
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	uq := NewUserQuery(WithName("alice"), WithAge(20))
	pp.Println(uq)

	var user User
	pp.Println(DoUserQuery(&user, WithName("alice"), WithAge(20)))
	return nil
}
