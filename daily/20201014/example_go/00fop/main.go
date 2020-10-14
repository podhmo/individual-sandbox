package main

import (
	"fmt"
	"reflect"
)

type Person struct {
	Name string // required
	Age  int    // unrequired
}

type personOption interface {
	Apply(p *Person)
}

type PersonOptional struct {
	Age int
}

type personOptionFunc func(*Person)

func (f personOptionFunc) Apply(p *Person) {
	f(p)
}
func WithAge(age int) personOptionFunc {
	return func(p *Person) {
		p.Age = age
	}
}

func (o PersonOptional) Apply(p *Person) {
	rsrc := reflect.ValueOf(o)
	rt := rsrc.Type()
	rdst := reflect.ValueOf(p).Elem()
	for i := 0; i < rt.NumField(); i++ {
		name := rt.Field(i).Name
		f := rdst.FieldByName(name)
		switch f.Kind() {
		case reflect.Ptr, reflect.Slice, reflect.Array, reflect.Map, reflect.Chan:
			if f.IsNil() {
				f.Set(rsrc.FieldByName(name))
			}
		default:
			if f.IsZero() {
				f.Set(rsrc.FieldByName(name))
			}
		}
	}
}

func Do(name string, options ...personOption) *Person {
	ob := &Person{
		Name: name,
	}
	for _, opt := range options {
		opt.Apply(ob)
	}
	return ob
}

func main() {
	fmt.Printf("%#+v\n", Do("foo"))
	fmt.Printf("%#+v\n", Do("foo", PersonOptional{Age: 20}))
	fmt.Printf("%#+v\n", Do("foo", WithAge(20)))
}
