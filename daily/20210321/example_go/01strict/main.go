package main

import (
	"encoding/json"
	"fmt"
	"log"
	"strconv"
)

// JSON: string, float64, bool のみ
// TODO: required
// TODO: pattern, enum, maxLength
// TODO: minLength,maxLength
// TODO: gt,gte,lt,lte

type Person struct {
	Name      string    `json:"name"`
	Children  []Person  `json:"children"`
	Children2 []*Person `json:"children2"`
}

type Message string

var (
	MsgInvalidType   Message = "invalid type is found"
	MsgRequired              = "required fields is not found"
	MsgUnknownFields         = "unknown fields are existed"
)

type Error struct {
	Path    []string
	Message Message
	Data    map[string]interface{}
}

func (ob *Person) BindMap(path []string, data map[string]interface{}) error {
	if v, ok := data["name"]; ok {
		if val, ok := v.(string); ok {
			ob.Name = val
			delete(data, "name")
		} else {
			return &Error{Path: append(path, "name"), Message: MsgInvalidType, Data: map[string]interface{}{"ExpectedType": "string", "ActualType": fmt.Sprintf("%T", v)}}
		}
	} else {
		return &Error{Path: append(path, "name"), Message: MsgRequired}
	}

	if vs, ok := data["children"]; ok {
		if val, ok := vs.([]interface{}); ok {
			xs := make([]Person, len(val))
			for i := range val {
				var x Person
				if err := BindMap(&x, append(path, strconv.Itoa(i)), val[i].(map[string]interface{})); err != nil {
					return err
				}
				xs[i] = x
			}
			ob.Children = xs
			delete(data, "children")
		} else {
			return &Error{Path: append(path, "children"), Message: MsgInvalidType, Data: map[string]interface{}{"ExpectedType": "slice", "Value": val}}
		}
	}

	if vs, ok := data["children2"]; ok {
		if val, ok := vs.([]interface{}); ok {
			xs := make([]*Person, len(val))
			for i := range val {
				var x Person
				if err := BindMap(&x, append(path, strconv.Itoa(i)), val[i].(map[string]interface{})); err != nil {
					return err
				}
				xs[i] = &x
			}
			ob.Children2 = xs
			delete(data, "children2")
		} else {
			return &Error{Path: append(path, "children2"), Message: MsgInvalidType, Data: map[string]interface{}{"ExpectedType": "slice", "Value": vs}}
		}
	}
	if len(data) > 0 {
		return &Error{Path: path, Message: MsgUnknownFields, Data: map[string]interface{}{"Value": data}}
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
