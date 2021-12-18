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

func (link *EmailLink) MarshalJSON() ([]byte, error) {
	type T EmailLink
	b, err := json.Marshal((*T)(link))
	if err != nil {
		return nil, err
	}

	// todo: optimize
	buf := bytes.NewBufferString(`{"$Type":"EmailLink","$Data":`)
	buf.Write(b)
	buf.WriteRune('}')
	return buf.Bytes(), nil
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
	// type Inner Account // このようにした場合でも埋め込みの方の処理の中でインターフェイスをunmarshalしようとして死ぬ
	type T struct {

		// ここのフィールドの定義をなくしたい
		Name         string
		RegisteredAt time.Time

		Primary *OneOfWrapper `json:"primary,omitempty"`
	}

	var w T
	if err := json.Unmarshal(b, &w); err != nil {
		return err
	}

	a.Name = w.Name
	a.RegisteredAt = w.RegisteredAt

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
