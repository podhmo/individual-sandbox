package main

import (
	"encoding/json"
	"log"

	"github.com/k0kubun/pp"
	"github.com/pkg/errors"
)

// User :
type User struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

// UnmarshalJSON :
func (u *User) UnmarshalJSON(b []byte) error {
	type userP struct {
		Name *string `json:"name"`
		Age  *int    `json:"age"`
	}

	var p userP
	if err := json.Unmarshal(b, &p); err != nil {
		return err
	}
	if p.Name == nil {
		return errors.Errorf("in User, name is not found data=%s", b)
	}
	u.Name = *p.Name
	if p.Age == nil {
		return errors.Errorf("in User, age is not found data=%s", b)
	}
	u.Age = *p.Age
	return nil
}

func main() {
	pp.ColoringEnabled = false

	users := []string{
		`{"name": "Ken", "age": 24}`,
		`{"name": "bob"}`,
	}

	{
		var u User
		if err := json.Unmarshal([]byte(users[0]), &u); err != nil {
			log.Fatal(err)
		}
		pp.Println(u)
	}
	{
		var u User
		if err := json.Unmarshal([]byte(users[1]), &u); err != nil {
			log.Fatal(err)
		}
		pp.Println(u)
	}
}
