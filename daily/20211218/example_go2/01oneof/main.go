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
	Link() string
}

// verified, unverified
// active, inactive(from user), killed(from system)

type EmailLink struct {
	EmailAddress string
	RegisteredAt time.Time
}

type OauthLink struct {
	ProfileID    string
	Source       string
	RegisteredAt time.Time
}

func (link *EmailLink) Link() string { return "EmailLink" }
func (link *OauthLink) Link() string { return "OauthLink" }

type Account struct {
	Name         string
	RegisteredAt time.Time

	Primary    Link       `json:"-"`
	RawPrimary *OneOfWrapper `json:"primary"`
	// Other   []Link
}

// 後で消すかもだけど。Unmarshalを楽にするために
type OneOfWrapper struct {
	Type string          `json:"$Type"`
	Raw  json.RawMessage `json:"$Data"`
}

func (a Account) MarshalJSON() ([]byte, error) {
	type T Account
	w := (*T)(&a)
	primary, err := json.Marshal(a.Primary)
	if err != nil {
		return nil, err
	}
	w.RawPrimary = &OneOfWrapper{Type: a.Primary.Link(), Raw: primary}
	return json.Marshal(w)
}

func (a *Account) UnmarshalJSON(b []byte) error {
	type T Account
	if err := json.Unmarshal(b, (*T)(a)); err != nil {
		return err
	}

	if err := unmarshalJSONLink(a.RawPrimary, &a.Primary); err != nil {
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
		return fmt.Errorf("unexpected one-of type: %s", data.Type)
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
		if err := json.NewDecoder(bytes.NewBufferString(s)).Decode(&foo); err != nil {
			panic(err)
		}
		pencode(foo)
	}
}

func dencode(s string, ob interface{}) {
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
