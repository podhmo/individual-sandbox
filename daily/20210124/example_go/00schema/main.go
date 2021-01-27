package main

type Person struct {
	Name string `json:"name" validation:"required maxLength:10 minLength:1"`
	Age  int    `json:"age" validation:"positive gt:10"`
}
