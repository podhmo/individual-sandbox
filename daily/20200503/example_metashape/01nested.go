package main

type Person struct {
	Name string
	Age int
	Father *Person
	Mother *Person
}
