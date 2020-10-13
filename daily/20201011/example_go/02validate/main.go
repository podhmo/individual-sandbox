package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"reflect"
	"strconv"
)

type Person struct {
	Name string `required:"true" json:"name"`
	Age  int    `required:"false" json:"age"`
}

func (p *Person) UnmarshalJSON(b []byte) error {
	// TODO: cache
	rv := reflect.ValueOf(p).Elem()
	rt := rv.Type()

	// TODO: omit `json:"-"`
	var fields []reflect.StructField
	required := map[string]bool{}
	for i := 0; i < rt.NumField(); i++ {
		f := rt.Field(i)
		ok, _ := strconv.ParseBool(f.Tag.Get("required"))
		required[f.Name] = ok
		fields = append(fields, reflect.StructField{
			Name: f.Name,
			Type: reflect.PtrTo(f.Type),
			Tag:  f.Tag,
		})
	}
	wt := reflect.StructOf(fields)
	ob := reflect.New(wt).Interface()

	if err := json.Unmarshal(b, ob); err != nil {
		return err
	}
	wrv := reflect.ValueOf(ob).Elem()
	for i, f := range fields {
		wrf := wrv.Field(i)
		if required[f.Name] && wrf.IsNil() {
			return fmt.Errorf("Field %q is required", f.Name)
		}
		if wrf.IsNil() {
			continue
		}

		rv.FieldByName(f.Name).Set(wrf.Elem())
	}
	return nil
}

func main() {
	{
		var p Person
		s := `{}`
		err := json.NewDecoder(bytes.NewBufferString(s)).Decode(&p)
		fmt.Printf("%#+v %+v\n", p, err)
	}
	{
		var p Person
		s := `{"name": ""}`
		err := json.NewDecoder(bytes.NewBufferString(s)).Decode(&p)
		fmt.Printf("%#+v %+v\n", p, err)
	}
	{
		var p Person
		s := `{"name": "foo"}`
		err := json.NewDecoder(bytes.NewBufferString(s)).Decode(&p)
		fmt.Printf("%#+v %+v\n", p, err)
	}
	{
		var p Person
		s := `{"name": "foo", "age": 20}`
		err := json.NewDecoder(bytes.NewBufferString(s)).Decode(&p)
		fmt.Printf("%#+v %+v\n", p, err)
	}
}
