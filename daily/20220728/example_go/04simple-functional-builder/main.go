package main

import "fmt"

type Person struct {
	Name     string
	Age      int
	Nickname string
}

type PersonF func() *Person

var NewPerson PersonF = func() *Person {
	return &Person{Age: 20}
}

func (f PersonF) WithName(name string) PersonF {
	return func() *Person {
		ob := f()
		ob.Name = name
		return ob
	}
}
func (f PersonF) WithAge(age int) PersonF {
	return func() *Person {
		ob := f()
		ob.Age = age
		return ob
	}
}

func main() {
	{
		ob := NewPerson()
		fmt.Printf("%+#v\n", ob) // &main.Person{Name:"", Age:20, Nickname:""}
	}
	{
		ob := NewPerson.WithAge(10).WithName("foo")()
		fmt.Printf("%+#v\n", ob) //	&main.Person{Name:"foo", Age:10, Nickname:""}
	}
}
