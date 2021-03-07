package main

import (
	"fmt"
	"net/http"
	"reflect"
)

type W struct {
	Name      string
	Transport http.RoundTripper
}

func Scan(rt reflect.Type, riface reflect.Type, n int) int {
	// todo: support by func
	if rt.Implements(riface) {
		n++
	}
	if rt.Kind() == reflect.Ptr {
		rt = rt.Elem()
		if rt.Implements(riface) {
			n++
		}
	}
	for i := 0; i < rt.NumField(); i++ {
		rf := rt.Field(i)
		rft := rf.Type
		if rft.Implements(riface) {
			fmt.Println(n, rf.Name, rf.Type)
			m := Scan(rf.Type, riface, n+1)
			if n < m {
				n = m
			}
		}
		if rft.Kind() == reflect.Ptr {
			rft = rf.Type.Elem()
			if rft.Implements(riface) {
				fmt.Println(n, rf.Name, rf.Type)
				m := Scan(rf.Type, riface, n)
				if n < m {
					n = m
				}
			}
		}
	}
	return n
}

func main() {
	// これではうまくいかないのか
	riface := reflect.TypeOf(func() http.RoundTripper { return nil }).Out(0)

	fmt.Println(Scan(reflect.TypeOf(struct{}{}), riface, 0))
	fmt.Println(Scan(reflect.TypeOf(http.DefaultTransport), riface, 0))
	{
		t := W{Transport: http.DefaultTransport}
		fmt.Println(Scan(reflect.TypeOf(t), riface, 0))
	}
}
