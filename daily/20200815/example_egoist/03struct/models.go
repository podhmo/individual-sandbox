package model

import (
	"github.com/podhmo/maperr"
	"encoding/json"
)

// this file is generated by egoist.generators.structkit

type Pereson struct {
	Name string `json:"name"`
	Age int `json:"age"`
}

func (p *Pereson) UnmarshalJSON(b []byte) error {
	var err *maperr.Error

	// loading internal data
	var inner struct {
		Name *string `json:"name"`// required
		Age *int `json:"age"`// required
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
		if inner.Age != nil  {
			p.Age = *inner.Age
		} else  {
			err = err.Add("age", maperr.Message{Text: "required"})
		}
	}

	return err.Untyped()
}