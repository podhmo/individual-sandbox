package main

import (
	"encoding/json"
	"fmt"
)

// http://go-talks.appspot.com/github.com/matryer/present/idiomatic-go-tricks/main.slide#18
type Person struct {
	Name string
}

type Valid interface {
	OK() error
}

func (p Person) OK() error {
	if p.Name == "" {
		return fmt.Errorf("name required: %s", p)
	}
	return nil
}

func Decoder(r io.Reader, v interface{}) error {
	err := json.NewDecoder(r).Decode(v)
	if err != nil {
		return err
	}

	obj, ok := v.(Valid)
	if !ok {
		return nil // no OK method
	}

	err = obj.OK()
	if err != nil {
		return err
	}
	return nil
}
