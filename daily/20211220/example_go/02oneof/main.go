package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"time"
)

// interfaceを含んだstructをJSONにして扱う (jsonpbのoneOfが使える場合があるが...)

type Link interface {
	Link()
}

type link struct {
	Type         string `json:"type"`
	RegisteredAt time.Time
}

func (*link) Link() {}

// verified, unverified
// active, inactive(from user), killed(from system)

type EmailLink struct {
	link
	EmailAddress string
}

func (link EmailLink) MarshalJSON() ([]byte, error) {
	type T EmailLink
	link.link.Type = "EmailLink"
	return json.Marshal((*T)(&link))
}

type OauthLink struct {
	link
	ProfileID string
	Source    string
}

type Account struct {
	Name         string
	RegisteredAt time.Time

	Primary Link
	// Other   []Link
}

func (a *Account) UnmarshalJSON(b []byte) error {
	type Inner Account
	type T struct {
		Primary json.RawMessage
		*Inner
	}
	w := T{Inner: (*Inner)(a)}
	if err := json.Unmarshal(b, &w); err != nil {
		return err
	}

	if err := unmarshalJSONLink(w.Primary, &a.Primary); err != nil {
		return err
	}
	return nil
}

func unmarshalJSONLink(b json.RawMessage, ref *Link) error {
	type T struct {
		Type string `json:"type"`
	}
	var t T
	if err := json.Unmarshal(b, &t); err != nil {
		return err
	}
	switch t.Type {
	case "EmailLink":
		var inner EmailLink
		*ref = &inner
		return json.Unmarshal(b, &inner)
	case "OauthLink":
		var inner OauthLink
		*ref = &inner
		return json.Unmarshal(b, &inner)
	default:
		return fmt.Errorf("unexpected interface=%q implementation, type=%q", "Link", t.Type)
	}
}

func main() {
	now := time.Now()
	{
		foo := Account{
			Name: "foo", RegisteredAt: now,
			Primary: &EmailLink{EmailAddress: "foo@example.net", link: link{RegisteredAt: now}},
		}
		pencode(foo)
	}
	{
		s := `
		{
		  "Name": "foo",
		  "RegisteredAt": "2021-12-18T19:14:03.755686099+09:00",
		  "Primary": {
			"type": "EmailLink",
			"EmailAddress": "foo@example.net",
			"RegisteredAt": "2021-12-18T19:14:03.755686099+09:00"
		  }
		}
		`
		var foo Account
		decode(s, &foo)
		pencode(foo)
	}
}

func decode(s string, ob interface{}) {
	if err := json.NewDecoder(bytes.NewBufferString(s)).Decode(ob); err != nil {
		panic(err)
	}
}

func pencode(ob interface{}) {
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(ob); err != nil {
		panic(err)
	}
}
