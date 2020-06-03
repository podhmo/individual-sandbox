package main

import (
	"encoding/json"
	"m/miniq"
	"os"
	"reflect"

	"github.com/k0kubun/pp"
)

var Book = struct {
	miniq.Table
	BookID    miniq.Int64Field
	Published miniq.Int64Field
	Title     miniq.StringField
	URL       miniq.StringField
}{
	Table:     miniq.Table("Book"),
	BookID:    miniq.Int64Field("bookId"),
	Published: miniq.Int64Field("published"),
	Title:     miniq.StringField("title"),
	URL:       miniq.StringField("url"),
}

func WithPrefix(dst, src interface{}, prefix string) error {
	// todo: use reflect?

	b, err := json.Marshal(src)
	if err != nil {
		return err
	}

	values := map[string]string{}
	if err := json.Unmarshal(b, &values); err != nil {
		return err
	}
	for k, v := range values {
		values[k] = prefix + v
	}

	b2, err := json.Marshal(values)
	if err != nil {
		return err
	}

	return json.Unmarshal(b2, dst)
}

func WithPrefix2(dst, src interface{}, prefix string) error {
	rsrc := reflect.ValueOf(src).Elem()
	rdst := reflect.ValueOf(dst).Elem()

	for i := 0; i < rsrc.NumField(); i++ {
		f := rdst.Field(i)
		if f.CanSet() && f.Kind() != reflect.String {
			continue
		}
		if !f.MethodByName("Name").IsValid() {
			continue // not Field
		}
		f.SetString(prefix + rsrc.Field(i).String())
	}
	return nil
}

func main() {
	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	encoder.Encode(&Book)

	// {
	// 	b := Book // shallow copy
	// 	WithPrefix(&b, &Book, "b.")
	// 	pp.Println(Book, "->", b)
	// }
	{
		b := Book // shallow copy
		WithPrefix2(&b, &Book, "b.")
		pp.Println(Book, "->", b)
	}
}
