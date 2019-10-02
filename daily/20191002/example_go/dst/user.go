package target

type User struct {
	Name string
}

// Hello :
// - 1
// - 2
func (u *User) Hello() string {
	return "Hello"
}

// Bye :**
func (u *User) Bye() string {
	return "Bye"
}

// Oops :
// - xxx
// - yyy
//
// hmm
// - 1
// - 2
func (u *User) Oops() string {
	return "Bye"
}
