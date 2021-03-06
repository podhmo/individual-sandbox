package main

import (
	"bytes"
	"encoding/json"
	"io"
	"log"

	"github.com/k0kubun/pp"
	"github.com/podhmo/maperr"
)

// this file is generated by egoist.generators.structkit

type Person struct {
	Name       string                       `json:"name"`
	Age        int                          `json:"age"`
	Followings [][]Person                   `json:"followings"`
	Groups     map[string]map[string]Person `json:"groups"`
}

func (p *Person) UnmarshalJSON(b []byte) error {
	var err *maperr.Error

	// loading internal data
	var inner struct {
		Name       *string          `json:"name"`       // required
		Age        *int             `json:"age"`        // required
		Followings *json.RawMessage `json:"followings"` // required
		Groups     *json.RawMessage `json:"groups"`     // required
	}
	if rawErr := json.Unmarshal(b, &inner); rawErr != nil {
		return err.AddSummary(rawErr.Error())
	}

	// binding field value and required check
	{
		if inner.Name != nil {
			p.Name = *inner.Name
		} else {
			err = err.Add("name", maperr.Message{Text: "required"})
		}
		if inner.Age != nil {
			p.Age = *inner.Age
		} else {
			err = err.Add("age", maperr.Message{Text: "required"})
		}
		if inner.Followings != nil {
			p.Followings = [][]Person{}
			if rawerr := json.Unmarshal(*inner.Followings, &p.Followings); rawerr != nil {
				err = err.Add("followings", maperr.Message{Error: rawerr})
			}
			// p.Followings = *inner.Followings
		} else {
			err = err.Add("followings", maperr.Message{Text: "required"})
		}
		if inner.Groups != nil {
			p.Groups = map[string]map[string]Person{}
			if rawerr := json.Unmarshal(*inner.Groups, &p.Groups); rawerr != nil {
				err = err.Add("groups", maperr.Message{Error: rawerr})
			}
			// p.Groups = *inner.Groups
		} else {
			err = err.Add("groups", maperr.Message{Text: "required"})
		}
	}

	return err.Untyped()
}

type Person2 struct {
	Name string `json:"name"`
}

func (p *Person2) UnmarshalJSON(b []byte) error {
	var err *maperr.Error

	// loading internal data
	var inner struct {
		Name *string `json:"name"` // required
	}
	if rawErr := json.Unmarshal(b, &inner); rawErr != nil {
		return err.AddSummary(rawErr.Error())
	}

	// binding field value and required check
	{
		if inner.Name != nil {
			p.Name = *inner.Name
		} else {
			err = err.Add("name", maperr.Message{Text: "required"})
		}
	}

	return err.Untyped()
}

// Load
func Load(r io.Reader, ob interface{}) error {
	decoder := json.NewDecoder(r)
	if err := decoder.Decode(ob); err != nil {
		_, ok := err.(*maperr.Error)
		if ok {
			return err
		}
		return (&maperr.Error{}).AddSummary(err.Error()).Untyped()
	}
	return nil
}

func main() {
	b := bytes.NewBufferString(`{"name": "foo","age": 20, "followings": [[{"name": "foo","age": 20, "followings": [], "groups": {}}]], "groups: {}}`)
	var p Person
	if err := Load(b, &p); err != nil {
		log.Fatalf("!! %+v", err)
	}
	pp.Println(p)
}
