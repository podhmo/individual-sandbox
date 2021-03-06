package main

import (
	"bytes"
	"encoding/json"
	"log"

	"github.com/k0kubun/pp"
	"github.com/podhmo/maperr"
)

type Person struct {
	Name string `json:"name"`
	Memo Memo   `json:"memo"`
}

func (p *Person) UnmarshalJSON(b []byte) error {
	var err *maperr.Error

	// loading internal data
	var inner struct {
		Name *string          `json:"name"` // required
		Memo *json.RawMessage `json:"memo"` // required
	}
	if rawErr := json.Unmarshal(b, &inner); rawErr != nil {
		return err.AddSummary(rawErr.Error())
	}

	// binding field value and required check
	if inner.Name != nil {
		p.Name = *inner.Name
	} else {
		err = err.Add("name", maperr.Message{Text: "required"})
	}
	if inner.Memo != nil {
		if rawerr := json.Unmarshal(*inner.Memo, &p.Memo); rawerr != nil {
			err = err.Add("memo", maperr.Message{Error: rawerr})
		}
	} else {
		err = err.Add("memo", maperr.Message{Text: "required"})
	}

	return err.Untyped()
}

type Memo struct {
	Name string `json:"name"`
}

func (m *Memo) UnmarshalJSON(b []byte) error {
	var err *maperr.Error

	// loading internal data
	var inner struct {
		Name *string `json:"name"` // required
	}
	if rawErr := json.Unmarshal(b, &inner); rawErr != nil {
		return err.AddSummary(rawErr.Error())
	}

	// binding field value and required check
	if inner.Name != nil {
		m.Name = *inner.Name
	} else {
		err = err.Add("name", maperr.Message{Text: "required"})
	}

	return err.Untyped()
}

func main() {
	b := bytes.NewBufferString(`{"name": "foo", "memo": {"name": "x"}}`)
	decoder := json.NewDecoder(b)

	var ob Person
	if err := decoder.Decode(&ob); err != nil {
		log.Fatalf("!! %+v", err)
	}

	pp.Println(ob)
}
