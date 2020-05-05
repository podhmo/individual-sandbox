package main

import (
	"github.com/podhmo/maperr"
	"encoding/json"
	"fmt"
	"strings"
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
	Kind MemoKind `json:"$kind"`
	X *X `json:"x,omitempty"`
	Y *Y `json:"y,omitempty"`
}

func (m *Memo) UnmarshalJSON(b []byte) error {
	var err *maperr.Error

	// loading internal data
	var inner struct {
		Kind *MemoKind `json:"$kind"`// required
		X *json.RawMessage `json:"X"`
		Y *json.RawMessage `json:"Y"`
	}
	if rawErr := json.Unmarshal(b, &inner); rawErr != nil  {
		return err.AddSummary(rawErr.Error())
	}

	// binding field value and required check
	if inner.Kind != nil  {
		m.Kind = *inner.Kind
	} else  {
		err = err.Add("$kind", maperr.Message{Text: "required"})
	}
	if inner.X != nil  {
		if rawerr := json.Unmarshal(*inner.X, &m.X); rawerr != nil  {
			err = err.Add("X", maperr.Message{Error: rawerr})
		}
	}
	if inner.Y != nil  {
		if rawerr := json.Unmarshal(*inner.Y, &m.Y); rawerr != nil  {
			err = err.Add("Y", maperr.Message{Error: rawerr})
		}
	}

	// one-of?
	{
		c := 0
		if m.X != nil  {
			c++
		}
		if m.Y != nil  {
			c++
		}
		if c != 1  {
			err.Add("$kind", maperr.Message{Text: "not one-of"})
		}
	}
	return err.Untyped()
}

type MemoKind string

const (
	MemoKindX MemoKind = "X"
	MemoKindY MemoKind = "Y"
)


func (v MemoKind) Valid() error {
	switch v {
	case MemoKindX, MemoKindY:
		return nil
	default:
		return fmt.Errorf("%q is invalid enum value of (X, Y)", v)
	}
}

func (v *MemoKind) UnmarshalJSON(b []byte) error {
	*v = MemoKind(strings.Trim(string(b), `"`))
	return v.Valid()
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
