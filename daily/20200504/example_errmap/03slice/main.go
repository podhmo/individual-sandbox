package main

import (
	"bytes"
	"encoding/json"
	"log"

	"github.com/k0kubun/pp"
	"github.com/podhmo/maperr"
)

type Person struct {
	Name      string    `json:"name"`
	Skils     []string  `json:"skils"`
	Children  []Person  `json:"children"`
	Children2 []*Person `json:"children2"`
	Children3 *[]Person `json:"children3"`
}

func (p *Person) UnmarshalJSON(b []byte) error {
	var err *maperr.Error

	// loading internal data
	var inner struct {
		Name      *string          `json:"name"`      // required
		Skils     *[]string        `json:"skils"`     // required
		Children  *json.RawMessage `json:"children"`  // required
		Children2 *json.RawMessage `json:"children2"` // required
		Children3 *json.RawMessage `json:"children3"`
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
	if inner.Skils != nil {
		p.Skils = *inner.Skils
	} else {
		err = err.Add("skils", maperr.Message{Text: "required"})
	}
	if inner.Children != nil {
		p.Children = []Person{}
		if rawerr := json.Unmarshal(*inner.Children, &p.Children); rawerr != nil {
			err = err.Add("children", maperr.Message{Error: rawerr})
		}
	} else {
		err = err.Add("children", maperr.Message{Text: "required"})
	}
	if inner.Children2 != nil {
		p.Children2 = []*Person{}
		if rawerr := json.Unmarshal(*inner.Children2, &p.Children2); rawerr != nil {
			err = err.Add("children2", maperr.Message{Error: rawerr})
		}
	} else {
		err = err.Add("children2", maperr.Message{Text: "required"})
	}
	if inner.Children3 != nil {
		p.Children3 = &[]Person{}
		if rawerr := json.Unmarshal(*inner.Children3, p.Children3); rawerr != nil {
			err = err.Add("children3", maperr.Message{Error: rawerr})
		}
	}

	return err.Untyped()
}

func main() {
	b := bytes.NewBufferString(`{"name": "foo", "children": [], "children2": [], "children3": [], "skils": []}`)
	decoder := json.NewDecoder(b)

	var ob Person
	if err := decoder.Decode(&ob); err != nil {
		log.Fatalf("!! %+v", err)
	}

	pp.Println(ob)
}
