package main

import (
	"github.com/podhmo/maperr"
	"encoding/json"
)

type Person struct {
	Name string `json:"name"`
	Father *Person `json:"father"`
	Mother *Person `json:"mother"`
}

func (p *Person) UnmarshalJSON(b []byte) error {
	var err *maperr.Error

	// loading internal data
	var inner struct {
		Name *string `json:"name"`// required
		Father *json.RawMessage `json:"father"`
		Mother *json.RawMessage `json:"mother"`
	}
	if rawErr := json.Unmarshal(b, &inner); rawErr != nil  {
		return err.AddSummary(rawErr.Error())
	}

	// binding field value and required check
	{
		if inner.Name != nil  {
			p.Name = *inner.Name
		} else  {
			err = err.Add("name", maperr.Message{Text: "required"})
		}
		if inner.Father != nil  {
			p.Father = &Person{}
			if rawerr := json.Unmarshal(*inner.Father, p.Father); rawerr != nil  {
				err = err.Add("father", maperr.Message{Error: rawerr})
			}
		}
		if inner.Mother != nil  {
			p.Mother = &Person{}
			if rawerr := json.Unmarshal(*inner.Mother, p.Mother); rawerr != nil  {
				err = err.Add("mother", maperr.Message{Error: rawerr})
			}
		}
	}

	return err.Untyped()
}

func main() {

}
