package main

import (
	"fmt"
	"reflect"
	"strconv"
	"strings"
)

type Person struct {
	ID     int
	Name   string `required:"true"`
	Father *Person
}

func validatePerson(ob Person) error {
	if ob.Name == "" {
		return fmt.Errorf("name is not zero")
	}
	if ob.Father != nil {
		if err := validatePerson(*ob.Father); err != nil {
			return fmt.Errorf("father -- %w", err)
		}
	}
	return nil
}

func validatePersonReflect(ob Person) error {
	rt := reflect.TypeOf(ob)
	rv := reflect.ValueOf(ob)

	// TODO: father
	for i := 0; i < rt.NumField(); i++ {
		rf := rt.Field(i)
		if ok, _ := strconv.ParseBool(rf.Tag.Get("required")); ok {
			if rv.Field(i).IsZero() {
				return fmt.Errorf("%s is not zero", rf.Name)
			}
		}
	}
	return nil
}

type Op struct {
	Op string
	S0 string
}

func validatePersonCode(ob Person) error {
	codeMap := map[string][]Op{
		"Person": {
			{Op: "field", S0: "Name"}, {Op: "isZero"}, {Op: "pop", S0: "field"},
			{Op: "field", S0: "Father"}, {Op: "pointer"}, {Op: "call", S0: "Person"}, {Op: "pop", S0: "pointer"}, {Op: "pop", S0: "field"},
			// {Op: "field", S0: "Children"}, {Op: "each", S0: "Person.Children"}, {Op: "pop", S0: "field"},
		},
		"Person.Children": {{Op: "call", S0: "Person"}},
	}
	return validateCode(ob, codeMap, codeMap["Person"])
}

func validateCode(ob interface{}, codeMap map[string][]Op, code []Op) error {
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
		case "pop":
			stack = stack[:len(stack)-1]
			current = stack[len(stack)-1]
		case "isZero":
			if current.value.IsZero() {
				return fmt.Errorf("%s is not zero", current.name) // TODO: field name
			}
		case "pointer":
			if !current.value.IsNil() {
				current = node{name: "*", value: current.value.Elem()}
				stack = append(stack, current)
			}
		case "call":
			if err := validateCode(current.value.Interface(), codeMap, codeMap[op.S0]); err != nil {
				xs := make([]string, 0, len(stack))
				for _, n := range stack {
					if n.name != "" {
						xs = append(xs, n.name)
					}
				}
				return fmt.Errorf("%s -- %w", strings.Join(xs, ", "), err)
			}

			// case "each":
		// 	xs := current.value
		// 	for i, n := 0, xs.Len(); i < n; i++ {
		// 		x := xs.Index(i)
		// 	}
		// case "eachMap":
		default:
			return fmt.Errorf("unexpected op: %q", op.Op)
		}
	}
	return nil
}

func main() {
	{
		ob := Person{}
		fmt.Println(ob, validatePerson(ob))
	}
	{
		ob := Person{Name: "foo", Father: &Person{}}
		fmt.Println(ob, validatePerson(ob))
	}
	fmt.Println("----------------------------------------")
	{
		ob := Person{}
		fmt.Println(ob, validatePersonReflect(ob))
	}
	fmt.Println("----------------------------------------")
	{
		ob := Person{}
		fmt.Println(ob, validatePersonCode(ob))
	}
	{
		ob := Person{Name: "foo", Father: &Person{}}
		fmt.Println(ob, validatePersonCode(ob))
	}
}
