package main

import (
	"github.com/podhmo/maperr"
	"encoding/json"
)

type Person struct {
	Name string `json:"name"`
	Memo Memo `json:"memo"`
}

func (p *Person) UnmarshalJSON(b []byte) error {
	var err *maperr.Error

	// loading internal data
	var inner struct {
		Name *string `json:"name"`// required
		Memo *json.RawMessage `json:"memo"`// required
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
	if inner.Memo != nil  {
		p.Memo = Memo{}
		if rawerr := json.Unmarshal(*inner.Memo, &p.Memo); rawerr != nil  {
			err = err.Add("memo", maperr.Message{Error: rawerr})
		}
	} else  {
		err = err.Add("memo", maperr.Message{Text: "required"})
	}

	return err.Untyped()
}

type Memo struct {
	Kind string `json:"$kind"`
	X *X `json:"x,omitempty"`
	Y *Y `json:"y,omitempty"`
}

type X struct {
	Name string `json:"name"`
	Xxx string `json:"xxx"`
}

func (x *X) UnmarshalJSON(b []byte) error {
	var err *maperr.Error

	// loading internal data
	var inner struct {
		Name *string `json:"name"`// required
		Xxx *string `json:"xxx"`// required
	}
	if rawErr := json.Unmarshal(b, &inner); rawErr != nil  {
		return err.AddSummary(rawErr.Error())
	}

	// binding field value and required check
	if inner.Name != nil  {
		x.Name = *inner.Name
	} else  {
		err = err.Add("name", maperr.Message{Text: "required"})
	}
	if inner.Xxx != nil  {
		x.Xxx = *inner.Xxx
	} else  {
		err = err.Add("xxx", maperr.Message{Text: "required"})
	}

	return err.Untyped()
}

type Y struct {
	Name string `json:"name"`
	Yyy string `json:"yyy"`
}

func (y *Y) UnmarshalJSON(b []byte) error {
	var err *maperr.Error

	// loading internal data
	var inner struct {
		Name *string `json:"name"`// required
		Yyy *string `json:"yyy"`// required
	}
	if rawErr := json.Unmarshal(b, &inner); rawErr != nil  {
		return err.AddSummary(rawErr.Error())
	}

	// binding field value and required check
	if inner.Name != nil  {
		y.Name = *inner.Name
	} else  {
		err = err.Add("name", maperr.Message{Text: "required"})
	}
	if inner.Yyy != nil  {
		y.Yyy = *inner.Yyy
	} else  {
		err = err.Add("yyy", maperr.Message{Text: "required"})
	}

	return err.Untyped()
}

func main() {

}
