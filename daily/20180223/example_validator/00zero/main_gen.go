package main

import (
	"encoding/json"
	"errors"
)

// UnmarshalJSON : (generated from ..User)
func (x *User) UnmarshalJSON(b []byte) error {
	type internal struct {
		Name *string `required:"true"`
		Age  *int    `required:"true"`
	}

	var p internal
	if err := json.Unmarshal(b, &p); err != nil {
		return err
	}

	if p.Name == nil {
		return errors.New("Name is required")
	}
	x.Name = *p.Name
	if p.Age == nil {
		return errors.New("Age is required")
	}
	x.Age = *p.Age
	return x.FormatCheck()
}

// FormatCheck : (generated from ..User)
func (x *User) FormatCheck() error {
	return nil
}
