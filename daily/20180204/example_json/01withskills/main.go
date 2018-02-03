package main

import (
	"encoding/json"
	"fmt"

	"github.com/k0kubun/pp"
	"github.com/pkg/errors"
)

// User :
type User struct {
	Name   string  `json:"name"`
	Age    int     `json:"age"`
	Skills []Skill `json:"skills"`
}

// Skill :
type Skill struct {
	Name string `json:"name"`
}

// UnmarshalJSON :
func (u *User) UnmarshalJSON(b []byte) error {
	type userP struct {
		Name   *string  `json:"name"`
		Age    *int     `json:"age"`
		Skills *[]Skill `json:"skills"`
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
	if p.Skills == nil {
		return errors.Errorf("in User, skills is not found data=%s", b)
	}
	u.Skills = *p.Skills
	return nil
}

// UnmarshalJSON :
func (u *Skill) UnmarshalJSON(b []byte) error {
	type skillP struct {
		Name *string `json:"name"`
	}

	var p skillP
	if err := json.Unmarshal(b, &p); err != nil {
		return err
	}
	if p.Name == nil {
		return errors.Errorf("in Skill, name is not found data=%s", b)
	}
	u.Name = *p.Name
	return nil
}

func main() {
	pp.ColoringEnabled = false

	users := []string{
		`{"name": "Ken", "age": 24, "skills": []}`,
		`{"name": "Ken", "age": 24}`,
		`{"name": "Ken", "age": 24, "skills": [{}]}`,
		`{"name": "bob"}`,
	}

	for _, s := range users {
		var u User
		fmt.Printf("input: %s\n", s)
		if err := json.Unmarshal([]byte(s), &u); err != nil {
			fmt.Println(err)
		} else {
			pp.Println(u)
		}
		fmt.Println("")
	}
}
