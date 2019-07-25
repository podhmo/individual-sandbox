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

// meta
type meta struct{}

// Excludes :
func (m meta) Excludes(wt reflect.Type, name, tagname string) map[string]bool {
	excludes := map[string]bool{}
	if f, ok := wt.FieldByName(name); ok {
		name := f.Tag.Get(tagname)
		for _, x := range strings.Split(name, ",") {
			excludes[x] = true
		}
	}
	return excludes
}

// Emit :
func (m meta) JSON(rt reflect.Type, rv reflect.Value, excludes map[string]bool) ([]byte, error) {
	var b bytes.Buffer
	b.WriteString("{")

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

// Wrap :
type Wrap struct {
	meta `exclude:"age"`
	*Person
}

// MarshalJSON :
func (w *Wrap) MarshalJSON() ([]byte, error) {
	rv := reflect.ValueOf(w.Person).Elem()
	rt := reflect.TypeOf(w.Person).Elem()
	wt := reflect.TypeOf(w).Elem()
	excludes := w.meta.Excludes(wt, "meta", "exclude")
	return w.meta.JSON(rt, rv, excludes)
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
