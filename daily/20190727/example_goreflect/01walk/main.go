package main

import (
	"fmt"
	"log"
	"strconv"
	"strings"

	"github.com/k0kubun/pp"
	"github.com/podhmo/go-webtest/jsonequal"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("+%v", err)
	}
}

type person struct {
	Name    string
	Age     int
	Parents []person
}

// Walk :
func Walk(
	v interface{},
	callback func(path []string, v interface{}),
) {
	var path []string
	walk(path, v, callback)
}

func walk(
	path []string,
	iface interface{},
	callback func(path []string, v interface{}),
) {
	switch val := iface.(type) {
	case map[string]interface{}:
		for k, v := range val {
			walk(append(path, k), v, callback)
		}
	case []interface{}:
		for i := range val {
			walk(append(path, strconv.Itoa(i)), val[i], callback)
		}
	default:
		callback(path, val)
	}
}

func run() error {
	father := person{
		Name: "foo",
		Age:  40,
	}
	mother := person{
		Name: "bar",
		Age:  40,
	}
	me := person{
		Name:    "far",
		Age:     20,
		Parents: []person{father, mother},
	}
	pp.Println(me)

	Walk(jsonequal.MustNormalize(me), func(path []string, v interface{}) {
		fmt.Println(strings.Repeat("  ", len(path)), path, "@", v)
	})
	return nil
}
