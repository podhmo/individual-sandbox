package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"runtime"

	"github.com/iancoleman/orderedmap"
	"github.com/podhmo/gos/enumgen"
	"github.com/podhmo/gos/openapigen"
)

func main() {
	log.SetFlags(0)

	if err := run(); err != nil {
		panic(err)
	}
}

func run() error {
	c := enumgen.DefaultConfig()
	b := enumgen.NewBuilder(c)

	Size := enumgen.Define("Size", b.String(
		b.StringValue("S").Name("Small"),
		b.StringValue("M").Name("Medium"),
		b.StringValue("L").Name("Large"),
	))

	Order := enumgen.Define("Order", b.String(
		b.StringValue("ASC"),
		b.StringValue("DESC"),
	)).Doc("順序").Default("ASC")

	_, here, _, _ := runtime.Caller(0)
	// go code
	{
		if err := os.MkdirAll(filepath.Join(filepath.Dir(here), "enum"), 0755); err != nil {
			return err
		}

		if err := WriteCode(filepath.Join(filepath.Dir(here), "enum/size.go"), c, Size); err != nil {
			return err
		}
		if err := WriteCode(filepath.Join(filepath.Dir(here), "enum/order.go"), c, Order); err != nil {
			return err
		}
	}

	// openapi
	{
		c := openapigen.DefaultConfig()
		b := openapigen.NewBuilder(c)

		openapigen.Define("Size", b.StringFromEnum(Size))
		openapigen.Define("Order", b.StringFromEnum(Order))

		doc, err := openapigen.ToSchema(b)
		if err != nil {
			return err
		}
		if err := WriteDoc(filepath.Join(filepath.Dir(here), "openapi.json"), doc); err != nil {
			return err
		}
	}

	return nil
}

func WriteCode(filename string, c *enumgen.Config, typ enumgen.EnumBuilder) error {
	log.Printf("write: %s", filename)
	var buf bytes.Buffer
	w := &buf

	fmt.Fprintf(w, "package %s\n", "enum")
	if err := c.ToGoCode(w, typ); err != nil {
		return err
	}
	return ioutil.WriteFile(filename, buf.Bytes(), 0644)
}

func WriteDoc(filename string, doc *orderedmap.OrderedMap) error {
	log.Printf("write: %s", filename)
	var buf bytes.Buffer
	w := &buf

	enc := json.NewEncoder(w)
	enc.SetIndent("", "  ")
	if err := enc.Encode(doc); err != nil {
		return err
	}

	return ioutil.WriteFile(filename, buf.Bytes(), 0644)
}
