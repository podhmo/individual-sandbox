package main

import (
	"github.com/podhmo/maperr"
	"encoding/json"
)

type Person struct {
	Name string `json:"name"`
	Children []Person `json:"children"`
	Children2 []*Person `json:"children2"`
}

func (p *Person) UnmarshalJSON(b []byte) error {
	var err *maperr.Error

	// loading internal data
	var inner struct {
		Name *string `json:"name"`// required
		Children *json.RawMessage `json:"children"`
		Children2 *json.RawMessage `json:"children2"`
	}
	if rawErr := json.Unmarshal(b, &inner); rawErr != nil  {
		return err.AddSummary(rawErr.Error())
	}

	// binding field value and required check
	if inner.Name != nil  {
		p.Name = *inner.Name
	} else  {
		err = err.Add("name", maperr.Message{Text: "required"})
	}
	if inner.Children != nil  {
		p.Children = []Person{}
		if rawerr := json.Unmarshal(*inner.Children, &p.Children); rawerr != nil  {
			err = err.Add("children", maperr.Message{Error: rawerr})
		}
	}
	if inner.Children2 != nil  {
		p.Children2 = []*Person{}
		if rawerr := json.Unmarshal(*inner.Children2, &p.Children2); rawerr != nil  {
			err = err.Add("children2", maperr.Message{Error: rawerr})
		}
	}

	return err.Untyped()
}

func main() {

}
