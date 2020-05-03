package main

type Person struct {
	Name string
	Age int
	X Person_X
}

type Person_X struct {
	Name string
	Y Person_X_Y
}

type Person_X_Y struct {
	Name string
}
