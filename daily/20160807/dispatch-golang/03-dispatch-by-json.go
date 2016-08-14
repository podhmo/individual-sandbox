package main

import (
	"encoding/json"
	"fmt"
)

type O struct {
	Name string
}

type A struct {
	Name string
}

type B struct {
	A
}

type C struct {
	*A
}

type D struct {
	A
}

type E struct {
	*A
}

func (a *A) MarshalJSON() ([]byte, error) {
	return []byte(`"a"`), nil
}

func (b *B) MarshalJSON() ([]byte, error) {
	return []byte(`"b"`), nil
}

func (c *C) MarshalJSON() ([]byte, error) {
	return []byte(`"c"`), nil
}

func (d D) MarshalJSON() ([]byte, error) {
	return []byte(`"d"`), nil
}

func (e E) MarshalJSON() ([]byte, error) {
	return []byte(`"e"`), nil
}

func main() {
	{
		target := O{Name: "foo"}
		{
			output, _ := json.Marshal(&target)
			fmt.Printf("o: %p, %s\n", &target, output)
		}
		{
			output, _ := json.Marshal(target)
			fmt.Printf("o: %p, %s\n", &target, output)
		}
	}
	fmt.Println("----------------------------------------")
	{
		target := A{Name: "foo"}
		{
			output, _ := json.Marshal(&target)
			fmt.Printf("a: %p, %s\n", &target, output)
		}
		{
			output, _ := json.Marshal(target)
			fmt.Printf("a: %p, %s\n", &target, output)
		}
	}
	fmt.Println("----------------------------------------")
	{
		target := B{A: A{Name: "foo"}}
		{
			output, _ := json.Marshal(&target)
			fmt.Printf("b: %p, %s\n", &target, output)
		}
		{
			output, _ := json.Marshal(target)
			fmt.Printf("b: %p, %s\n", &target, output)
		}
	}
	fmt.Println("----------------------------------------")
	{
		target := C{A: &A{Name: "foo"}}
		{
			output, _ := json.Marshal(&target)
			fmt.Printf("c: %p, %s\n", &target, output)
		}
		{
			output, _ := json.Marshal(target)
			fmt.Printf("c: %p, %s\n", &target, output)
		}
	}
	fmt.Println("----------------------------------------")
	{
		target := D{A: A{Name: "foo"}}
		{
			output, _ := json.Marshal(&target)
			fmt.Printf("d: %p, %s\n", &target, output)
		}
		{
			output, _ := json.Marshal(target)
			fmt.Printf("d: %p, %s\n", &target, output)
		}
	}
	fmt.Println("----------------------------------------")
	{
		target := E{A: &A{Name: "foo"}}
		{
			output, _ := json.Marshal(&target)
			fmt.Printf("e: %p, %s\n", &target, output)
		}
		{
			output, _ := json.Marshal(target)
			fmt.Printf("e: %p, %s\n", &target, output)
		}
	}
}
