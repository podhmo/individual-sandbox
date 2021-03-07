package main

import (
	"fmt"
	"net/http"
	"reflect"
)

func main() {
	rt := reflect.TypeOf(http.DefaultTransport)
	iface := reflect.TypeOf(func() http.RoundTripper { return nil }).Out(0)
	fmt.Println(rt.Implements(iface))
}
