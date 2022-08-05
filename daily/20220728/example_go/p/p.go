package p

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

func (f PersonF) WithName(name string) PersonF {
	return func(options ...personOption) *Person {
		return f(append([]personOption{
			func(ob *Person) { ob.Name = name }},
			options...)...)
	}
}
func (f PersonF) WithAge(age int) PersonF {
	return func(options ...personOption) *Person {
		return f(append([]personOption{
			func(ob *Person) { ob.Age = age }},
			options...)...)
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
		return f(append([]teamOption{
			func(ob *Team) { ob.Name = name }},
			options...)...)
	}
}
