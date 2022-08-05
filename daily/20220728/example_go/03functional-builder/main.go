package main

import "fmt"

type Person struct {
	Name     string
	Age      int
	Nickname string
}

type personOption = func(*Person)
type PersonF func(...personOption) *Person

var NewPerson PersonF = func(options ...personOption) *Person {
	ob := &Person{Age: 20}
	for _, opt := range options {
		opt(ob)
	}
	return ob
}

func prepend[T any](xs []T, x T) []T {
	return append([]T{x}, xs...)
}

func (f PersonF) WithName(name string) PersonF {
	return func(options ...personOption) *Person {
		return f(prepend(options, func(ob *Person) { ob.Name = name })...)
	}
}
func (f PersonF) WithAge(age int) PersonF {
	return func(options ...personOption) *Person {
		return f(prepend(options, func(ob *Person) { ob.Age = age })...)
	}
}

// ----------------------------------------
type Team struct {
	Name     string
	Nickname string
}

type teamOption = func(*Team)
type TeamF func(...teamOption) *Team

var NewTeam TeamF = func(options ...teamOption) *Team {
	ob := &Team{}
	for _, opt := range options {
		opt(ob)
	}
	return ob
}

func (f TeamF) WithName(name string) TeamF {
	return func(options ...teamOption) *Team {
		return f(prepend(options, func(ob *Team) { ob.Name = name })...)
	}
}

func main() {
	{
		ob := NewPerson()
		fmt.Printf("%#+v\n", ob)
	}

	{
		ob := NewPerson.WithName("foo")()
		fmt.Printf("%#+v\n", ob)
	}

	{
		ob := NewPerson.WithName("foo")(func(p *Person) {
			p.Name = "boo"
			p.Nickname = "B"
		})
		fmt.Printf("%#+v\n", ob)
	}

	fmt.Println("----------------------------------------")
	{
		ob := NewTeam.WithName("foo")()
		fmt.Printf("%#+v\n", ob)
	}
	{
		ob := NewTeam.WithName("foo")(func(ob *Team) {
			ob.Name = "bar"
			ob.Nickname = "B"
		})
		fmt.Printf("%#+v\n", ob)
	}
}

// &main.Person{Name:"", Age:20, Nickname:""}
// &main.Person{Name:"foo", Age:20, Nickname:""}
// &main.Person{Name:"boo", Age:20, Nickname:"B"}
// ----------------------------------------
// &main.Team{Name:"foo", Nickname:""}
// &main.Team{Name:"bar", Nickname:"B"}
