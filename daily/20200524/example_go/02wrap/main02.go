package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
)

type User struct {
	ID       int    `json:"id"`
	TeamID   int    `json:"teamId"`
	Name     string `json:"name"`
	Age      int    `json:"age"`
	Password string `json:"password"` // hidden
}

type Team struct {
	ID   int    `json:"id"`
	Name string `json:"name"`
}

type UserOutput struct {
	User *User
	Team *TeamOutput
}

// todo: performance

func (o *UserOutput) MarshalJSON() ([]byte, error) {
	b := bytes.NewBuffer(nil)
	hasComma := true

	b.WriteByte('{')
	if o.User != nil {
		fmt.Fprintf(b, `"id":%d,`, o.User.ID)
		fmt.Fprintf(b, `"name":%q,`, o.User.Name)
		fmt.Fprintf(b, `"age":%d`, o.User.Age)
		hasComma = false
	}
	if o.Team != nil {
		if !hasComma {
			b.WriteString(",")
		}
		b.WriteString(`"team":`)
		sb, err := o.Team.MarshalJSON()
		if err != nil {
			return nil, err
		}
		b.Write(sb)
		hasComma = false
	}
	b.WriteByte('}')
	return b.Bytes(), nil
}

type TeamOutput struct {
	*Team
}

func (o *TeamOutput) MarshalJSON() ([]byte, error) {
	if o.Team == nil {
		return nil, nil
	}

	b := bytes.NewBuffer(nil)
	b.WriteByte('{')
	if o.Team != nil {
		fmt.Fprintf(b, `"id":%d,`, o.Team.ID)
		fmt.Fprintf(b, `"name":%q`, o.Team.Name)
	}
	b.WriteByte('}')
	return b.Bytes(), nil
}

func main() {
	user := User{Name: "foo", Age: 20, Password: "hidden"}
	team := Team{Name: "xxx"}

	output := UserOutput{
		User: &user,
		Team: &TeamOutput{&team},
	}

	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	if err := encoder.Encode(&output); err != nil {
		panic(err)
	}
}
