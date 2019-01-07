package main

import (
	"fmt"

	"github.com/pkg/errors"
)

// User :
type User struct {
	ID   string
	Name string
}

// ErrNotFound :
type ErrNotFound struct {
	ID string
}

// Error :
func (e *ErrNotFound) Error() string {
	return fmt.Sprintf("not found (id=%s)", e.ID)
}

var (
	pool map[string]*User
)

// GetUser :
func GetUser(ID string) (*User, error) {
	user, ok := pool[ID]
	if !ok {
		return nil, errors.Wrap(&ErrNotFound{ID: ID}, "on get user")
	}
	return user, nil
}

// BindUser :
func BindUser(ID string, ref *User) error {
	var ok bool
	user, ok := pool[ID]
	if !ok {
		return errors.Wrap(&ErrNotFound{ID: ID}, "on bind user")
	}
	*ref = *user
	return nil
}

func main() {
	{
		message := "get user"
		fmt.Println(message)

		user1, err := GetUser("0")
		if err != nil {
			fmt.Printf("user1 %s\n", err)
		}
		user3, err := GetUser("3")
		if err != nil {
			fmt.Printf("user3 %s\n", err)
		}
		fmt.Printf("	(%#v, %#v)\n", user1, user3)
		fmt.Println("!!!", err)
	}
	fmt.Println("----------------------------------------")
	{
		message := "bind user"
		fmt.Println(message)

		var user1 User
		var user3 User
		if err := BindUser("1", &user1); err != nil {
			fmt.Printf("user1 %s\n", err)
		}
		if err := BindUser("3", &user3); err != nil {
			fmt.Printf("user3 %s\n", err)
		}
		fmt.Printf("	(%#v, %#v)\n", user1, user3)
	}
}

func init() {
	pool = map[string]*User{}
	{
		user := &User{
			ID:   "1",
			Name: "foo",
		}
		pool[user.ID] = user
	}
	{
		user := &User{
			ID:   "2",
			Name: "bar",
		}
		pool[user.ID] = user
	}
}
