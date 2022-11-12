package main

import (
	"fmt"
	"reflect"
	"strconv"
	"testing"

	"github.com/gorilla/schema"
)

type Person struct {
	Name  string `schema:"name,required"` // custom name, must be supplied
	Phone string `schema:"phone"`         // custom name
	Age   int    `schema:"age"`
	Admin bool   `schema:"-"` // this field is never set
}

func BenchmarkGorillaSchema(b *testing.B) {
	decoder := schema.NewDecoder()
	data := map[string][]string{"name": {"foo"}, "phone": {""}, "age": {"10"}}
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		var ob Person
		if err := decoder.Decode(&ob, data); err != nil {
			panic(err)
		}
	}
}

func BenchmarkByHand(b *testing.B) {
	data := map[string][]string{"name": {"foo"}, "phone": {""}, "age": {"10"}}
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		var ob Person
		if err := BindPerson(&ob, data); err != nil {
			panic(err)
		}
	}
}

func BindPerson(ob *Person, data map[string][]string) error {
	if v, ok := data["name"]; !ok {
		return fmt.Errorf("hmm: %v", "name")
	} else {
		ob.Name = v[0]
	}
	if v, ok := data["phone"]; ok {
		ob.Phone = v[0]
	}
	if v, ok := data["age"]; ok {
		if v, err := strconv.Atoi(v[0]); err != nil {
			return fmt.Errorf("hmm: %v -- %w", "age", err)
		} else {
			ob.Age = v
		}
	}
	return nil
}

func BenchmarkByHandReflect(b *testing.B) {
	data := map[string][]string{"name": {"foo"}, "phone": {""}, "age": {"10"}}
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		var ob Person
		if err := BindPersonReflect(&ob, data); err != nil {
			panic(err)
		}
	}
}

func BindPersonReflect(ob *Person, data map[string][]string) error {
	rv := reflect.ValueOf(ob).Elem()
	if v, ok := data["name"]; !ok {
		return fmt.Errorf("hmm: %v", "name")
	} else {
		rv.Field(0).SetString(v[0])
	}
	if v, ok := data["phone"]; ok {
		rv.Field(1).SetString(v[0])
	}
	if v, ok := data["age"]; ok {
		if v, err := strconv.ParseInt(v[0], 10, 0); err != nil {
			return fmt.Errorf("hmm: %v -- %w", "age", err)
		} else {
			rv.Field(2).SetInt(v)
		}
	}
	return nil
}

func BenchmarkByHandReflectVM(b *testing.B) {
	data := map[string][]string{"name": {"foo"}, "phone": {""}, "age": {"10"}}
	b.ResetTimer()

	codeMap := map[string][]Op{
		"Person": {
			{Op: "pointer"},
			{Op: "field-index", S0: "Name", I0: 0}, {Op: "setString", S0: "name"}, {Op: "isZero"}, {Op: "pop", S0: "field"},
			{Op: "field-index", S0: "Phone", I0: 1}, {Op: "setString", S0: "phone"}, {Op: "pop", S0: "field"},
			{Op: "field-index", S0: "Age", I0: 2}, {Op: "setInt", S0: "age"}, {Op: "pop", S0: "field"},
		},
	}

	for i := 0; i < b.N; i++ {
		var ob Person
		if err := BindByCode(&ob, codeMap, codeMap["Person"], data); err != nil {
			panic(err)
		}
	}
}

type Op struct {
	Op string
	S0 string
	I0 int
}

func BindByCode(ob interface{}, codeMap map[string][]Op, code []Op, data map[string][]string) error {
	type node struct {
		name  string
		value reflect.Value
	}
	stack := []node{{value: reflect.ValueOf(ob)}}
	current := stack[0]
	for _, op := range code {
		switch op.Op {
		case "field":
			current = node{name: op.S0, value: current.value.FieldByName(op.S0)}
			stack = append(stack, current)
		case "field-index":
			current = node{name: op.S0, value: current.value.Field(op.I0)}
			stack = append(stack, current)
		case "pop":
			stack = stack[:len(stack)-1]
			current = stack[len(stack)-1]
		case "isZero":
			if current.value.IsZero() {
				return fmt.Errorf("%s is not zero", current.name)
			}
		case "pointer":
			if !current.value.IsNil() {
				current = node{name: "*", value: current.value.Elem()}
				stack = append(stack, current)
			}
		case "setString":
			if v, ok := data[op.S0]; ok {
				current.value.SetString(v[0])
			}
		case "setInt":
			if v, ok := data[op.S0]; ok {
				if v, err := strconv.ParseInt(v[0], 10, 0); err != nil {
					return fmt.Errorf("hmm: %v -- %w", "age", err)
				} else {
					current.value.SetInt(v)
				}
			}
		default:
			return fmt.Errorf("unexpected op: %q", op.Op)
		}
	}
	return nil
}

func TestIt(t *testing.T) {
	t.Run("gorilla/schema", func(t *testing.T) {
		decoder := schema.NewDecoder()
		data := map[string][]string{"name": {"foo"}, "phone": {""}, "age": {"10"}}

		var ob Person
		if err := decoder.Decode(&ob, data); err != nil {
			t.Fatal(err)
		}
		t.Logf("%#+v", ob)
	})
	t.Run("byhand", func(t *testing.T) {
		data := map[string][]string{"name": {"foo"}, "phone": {""}, "age": {"10"}}

		var ob Person
		if err := BindPerson(&ob, data); err != nil {
			t.Fatal(err)
		}
		t.Logf("%#+v", ob)
	})
	t.Run("byhand-reflect", func(t *testing.T) {
		data := map[string][]string{"name": {"foo"}, "phone": {""}, "age": {"10"}}

		var ob Person
		if err := BindPersonReflect(&ob, data); err != nil {
			t.Fatal(err)
		}
		t.Logf("%#+v", ob)
	})
	t.Run("byhand-reflect-vm", func(t *testing.T) {
		data := map[string][]string{"name": {"foo"}, "phone": {""}, "age": {"10"}}
		codeMap := map[string][]Op{
			"Person": {
				{Op: "pointer"},
				{Op: "field-index", S0: "Name", I0: 0}, {Op: "setString", S0: "name"}, {Op: "isZero"}, {Op: "pop", S0: "field"},
				{Op: "field-index", S0: "Phone", I0: 1}, {Op: "setString", S0: "phone"}, {Op: "pop", S0: "field"},
				{Op: "field-index", S0: "Age", I0: 2}, {Op: "setInt", S0: "age"}, {Op: "pop", S0: "field"},
			},
		}

		var ob Person
		if err := BindByCode(&ob, codeMap, codeMap["Person"], data); err != nil {
			t.Fatal(err)
		}
		t.Logf("%#+v", ob)
	})
}
