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

// verified, unverified
// active, inactive(from user), killed(from system)

type EmailLink struct {
	EmailAddress string
	RegisteredAt time.Time
}

func (link EmailLink) MarshalJSON() ([]byte, error) {
	type Inner EmailLink
	type T struct {
		Type string `json:"$Type"`
		Data *Inner `json:"$Data"`
	}
	return json.Marshal(&T{Type: "EmailLink", Data: (*Inner)(&link)})
}

type OauthLink struct {
	ProfileID    string
	Source       string
	RegisteredAt time.Time
}

func (link *EmailLink) Link() {}
func (link *OauthLink) Link() {}

type Account struct {
	Name         string
	RegisteredAt time.Time

	Primary Link
	// Other   []Link
}

// 後で消すかもだけど。Unmarshalを楽にするために
type OneOfWrapper struct {
	Type string          `json:"$Type"`
	Raw  json.RawMessage `json:"$Data"`
}

func (a *Account) UnmarshalJSON(b []byte) error {
	type Inner Account
	type T struct {
		Primary *OneOfWrapper
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

func unmarshalJSONLink(data *OneOfWrapper, ref *Link) error {
	if data == nil {
		return nil
	}

	switch data.Type {
	case "EmailLink":
		var inner EmailLink
		*ref = &inner
		return json.Unmarshal(data.Raw, &inner)
	case "OauthLink":
		var inner OauthLink
		*ref = &inner
		return json.Unmarshal(data.Raw, &inner)
	default:
		return fmt.Errorf("unexpected interface=%q implementation, type=%q", "Link", data.Type)
	}
}

func main() {
	now := time.Now()
	{
		foo := Account{
			Name: "foo", RegisteredAt: now,
			Primary: &EmailLink{EmailAddress: "foo@example.net", RegisteredAt: now},
		}
		pencode(foo)
	}
	{
		s := `
		{
		  "Name": "foo",
		  "RegisteredAt": "2021-12-18T19:14:03.755686099+09:00",
		  "Primary": {
			"$Type": "EmailLink",
			"$Data": {
			  "EmailAddress": "foo@example.net",
			  "RegisteredAt": "2021-12-18T19:14:03.755686099+09:00"
			}
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
