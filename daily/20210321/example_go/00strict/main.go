package main

import (
	"encoding/json"
	"fmt"
	"log"
	"strconv"
)

// JSON: string, float64, bool のみ

type Person struct {
	Name      string    `json:"name"`
	Children  []Person  `json:"children"`
	Children2 []*Person `json:"children2"`
}

func (ob *Person) BindMap(path []string, data map[string]interface{}) error {
	if v, ok := data["name"]; ok {
		if v, ok := v.(string); ok {
			ob.Name = v
			delete(data, "name")
		} else {
			return &Error{Path: append(path, "name"), Message: fmt.Sprintf("unexpected type, expected type is string, but %v is got", v)}
		}
	} else {
		return &Error{Path: append(path, "name"), Message: "required"}
	}

	if vs, ok := data["children"]; ok {
		if vs, ok := vs.([]interface{}); ok {
			xs := make([]Person, len(vs))
			for i := range vs {
				var x Person
				if err := BindMap(&x, append(path, strconv.Itoa(i)), vs[i].(map[string]interface{})); err != nil {
					return err
				}
				xs[i] = x
			}
			ob.Children = xs
			delete(data, "children")
		} else {
			return &Error{Path: append(path, "children"), Message: "unexpected type, slice is expected"}
		}
	}

	if vs, ok := data["children2"]; ok {
		if vs, ok := vs.([]interface{}); ok {
			xs := make([]*Person, len(vs))
			for i := range vs {
				var x Person
				if err := BindMap(&x, append(path, strconv.Itoa(i)), vs[i].(map[string]interface{})); err != nil {
					return err
				}
				xs[i] = &x
			}
			ob.Children2 = xs
			delete(data, "children2")
		} else {
			return &Error{Path: append(path, "children2"), Message: "unexpected type, slice is expected"}
		}
	}
	if len(data) > 0 {
		keys := make([]string, 0, len(data))
		for k := range data {
			keys = append(keys, k)
		}
		return &Error{Path: path, Message: fmt.Sprintf("unexpected fields are found %v", keys)}
	}
	return nil
}

type Binder interface {
	BindMap(path []string, data map[string]interface{}) error
}

func BindMap(ob interface{}, path []string, data map[string]interface{}) error {
	t, ok := ob.(Binder)
	if !ok {
		return &Error{Path: path[:], Message: "method BindMap() is not implemented"}
	}
	return t.BindMap(path, data)
}

type Error struct {
	Path    []string
	Message string
}

func (e *Error) Error() string {
	return fmt.Sprintf("path=%[2]s, %[1]s", e.Message, e.Path)
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	// s := `{"name": 100}`
	// s := `{}`
	// s := `{"name": "foo"}`
	// s := `{"name": "foo", "children": [{"name": "bar"}]}`
	// s := `{"name": "foo", "children": [{"name": "bar"}], "children2": [{"name": "bar2"}]}`
	s := `{"name": "foo", "children": [{"name": "bar"}], "children2": [{"name": "bar2"}], "xxx": "yyy"}`
	data := map[string]interface{}{}
	if err := json.Unmarshal([]byte(s), &data); err != nil {
		return err
	}
	var ob Person
	if err := BindMap(&ob, nil, data); err != nil {
		return err
	}
	fmt.Printf("%+#v\n", ob)
	return nil
}
