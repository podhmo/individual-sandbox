package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"reflect"
	"sync"
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

type P struct {
	Inner reflect.Type
	Outer reflect.Type
}

// e.g.
// type Inner <Type>
// type Outer struct {
// 	*Inner
// 	Primary *OneOfWrapper `json:"primary,omitempty"` // e.g.
// }

var (
	typeCacheMap = map[reflect.Type]P{}
	mu           sync.RWMutex
)

func GetOrCreateNewType(rt reflect.Type) P {
	mu.RLock()
	if p, ok := typeCacheMap[rt]; ok {
		mu.RUnlock()
		return p
	}
	mu.RUnlock()
	mu.Lock()
	defer mu.Unlock()

	// TODO: pointer handling (素直にmap [string]interface{} でごまかすのが楽なのでは？)

	fields := make([]reflect.StructField, 0, rt.NumField())
	ifaceFields := make([]reflect.StructField, 0, rt.NumField())
	for i := 0; i < rt.NumField(); i++ {
		f := rt.Field(i)
		if f.Type.Kind() != reflect.Interface {
			fields = append(fields, f)
			continue
		}
		ifaceFields = append(ifaceFields, f)
		f.Tag = reflect.StructTag(`json:"-"`) // 本当なら真面目にtagを
		fields = append(fields, f)
	}
	innerType := reflect.StructOf(fields)

	wrapperType := reflect.TypeOf(&OneOfWrapper{}) // todo: cache?
	outerFields := make([]reflect.StructField, len(ifaceFields)+1)
	outerFields[0] = reflect.StructField{
		Type:      reflect.PtrTo(innerType),
		Name:      rt.Name(),
		Anonymous: true,
	}
	for i, ifaceField := range ifaceFields {
		outerFields[i+1] = reflect.StructField{
			Name: ifaceField.Name,
			Type: wrapperType,
			// 本当なら真面目にtagを
		}
	}
	outerType := reflect.StructOf(outerFields)

	p := P{Inner: reflect.PtrTo(innerType), Outer: reflect.PtrTo(outerType)}
	typeCacheMap[rt] = p
	return p
}

func (a *Account) UnmarshalJSON(b []byte) error {
	rv := reflect.ValueOf(a)
	p := GetOrCreateNewType(rv.Type().Elem())

	rw := reflect.New(p.Outer.Elem())
	rw.Elem().Field(0).Set(rv.Convert(p.Inner)) // Account
	w := rw.Interface()
	if err := json.Unmarshal(b, w); err != nil {
		return err
	}

	if err := unmarshalJSONLink(rw.Elem().FieldByName("Primary").Interface().(*OneOfWrapper), &a.Primary); err != nil {
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
