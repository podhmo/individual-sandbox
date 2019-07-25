package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"reflect"
	"strings"
)

// Person :
type Person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

// Wrap :
type Wrap struct {
	meta struct{} `exclude:"age"`
	*Person
}

// MarshalJSON :
func (w *Wrap) MarshalJSON() ([]byte, error) {
	var b bytes.Buffer
	rv := reflect.ValueOf(w.Person).Elem()
	rt := reflect.TypeOf(w.Person).Elem()
	b.WriteString("{")

	excludes := map[string]bool{}
	wt := reflect.TypeOf(w).Elem()

	for i, n := 0, wt.NumField(); i < n; i++ {
		name := wt.Field(i).Tag.Get("exclude")
		if name == "" {
			continue
		}
		for _, x := range strings.Split(name, ",") {
			excludes[x] = true
		}
	}

	for i, n := 0, rt.NumField(); i < n; i++ {
		name := rt.Field(i).Tag.Get("json")
		if name == "-" {
			continue
		}
		if _, exists := excludes[name]; exists {
			continue
		}

		fmt.Fprintf(&b, `"%s":`, name)
		v := rv.Field(i)
		switch v.Kind() {
		case reflect.Bool:
			fmt.Fprintf(&b, `%v,`, v.Bool())
		case reflect.Int:
			fmt.Fprintf(&b, `%d,`, v.Int())
		case reflect.Float64, reflect.Float32:
			fmt.Fprintf(&b, `%f,`, v.Float())
		case reflect.String:
			fmt.Fprintf(&b, `"%s",`, v.String())
		default:
			// hmm
		}
	}
	b.Truncate(b.Len() - 1)
	b.WriteString("}")
	return b.Bytes(), nil
}

func main() {
	p := Person{Name: "foo", Age: 20}
	encoder := json.NewEncoder(os.Stdout)

	if err := encoder.Encode(&p); err != nil {
		panic(err)
	}
	fmt.Println("----------------------------------------")
	if err := encoder.Encode(&Wrap{Person: &p}); err != nil {
		panic(err)
	}
}
