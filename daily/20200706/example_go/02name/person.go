package person

type Person struct {
	Name string
	Age  int
}

type Team struct {
	Name    string
	Members []Person
}
