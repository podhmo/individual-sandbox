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
	Info *Info  `json:"info"`
}

func (p *Person) UnmarshalJSON(b []byte) error {
	var err *maperr.Error

	// loading internal data
	var inner struct {
		Name *string `json:"name"` // required
		Info **Info  `json:"info"`
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
	if inner.Info != nil {
		p.Info = *inner.Info
	}
	return err.Untyped()
}

type Info struct {
	Name string `json:"name"`
}

func (i *Info) UnmarshalJSON(b []byte) error {
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
		i.Name = *inner.Name
	} else {
		err = err.Add("name", maperr.Message{Text: "required"})
	}
	return err.Untyped()
}

func main() {
	b := bytes.NewBufferString(`{"name": "foo", "info": {}}`)
	decoder := json.NewDecoder(b)

	var ob Person
	if err := decoder.Decode(&ob); err != nil {
		log.Fatalf("!! %+v", err)
	}
	pp.Println(ob)
}
