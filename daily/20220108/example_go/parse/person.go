package parse

import (
	"encoding/json"
	"fmt"
)

type person struct {
	Name string `json:"name"`
}
type Person struct {
	*person
}

func (p *person) validate() error {
	if p.Name == "" {
		return fmt.Errorf("name is required")
	}
	return nil
}

func Parse(s string) (*Person, error) {
	var p person
	if err := json.Unmarshal([]byte(s), &p); err != nil {
		return nil, err
	}
	if err := p.validate(); err != nil {
		return nil, err
	}
	return &Person{person: &p}, nil
}
