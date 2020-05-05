package main

import (
	"github.com/podhmo/maperr"
	"encoding/json"
)

type Person struct {
	Name string `json:"name"`
}

func (p *Person) UnmarshalJSON(b []byte) error {
	var err *maperr.Error

	// loading internal data
	var inner struct {
		Name *string `json:"name"`// required
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
	}

	return err.Untyped()
}

func main() {

}
