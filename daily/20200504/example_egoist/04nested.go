package main

import (
	"github.com/podhmo/errmap"
	"encoding/json"
)

type Person struct {
	Name string `json:"name"`
	Father *Person `json:"father"`
	Mother *Person `json:"mother"`
}

func (p *Person) UnmarshalJSON(b []byte) error {
	var err *errmap.Error

	// loading internal data
	var inner struct {
		Name *string `json:"name"`// required
		Father **Person `json:"father"`
		Mother **Person `json:"mother"`
	}
	if rawErr := json.Unmarshal(b, &inner); rawErr != nil  {
		return err.addSummary(rawErr.Error())
	}

	// binding field value and required check
	if inner.Name != nil  {
		p.Name = *inner.Name
	} else  {
		err = err.Add("name", "required")
	}
	if inner.Father != nil  {
		p.Father = *inner.Father
	}
	if inner.Mother != nil  {
		p.Mother = *inner.Mother
	}

	return err.Untyped()
}
