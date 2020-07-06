package person

type Name string
type Age int
type Age2 Age

type Person struct {
	Name Name
	Age  Age2
}
