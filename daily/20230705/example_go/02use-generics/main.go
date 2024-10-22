package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"runtime"

	"github.com/podhmo/gos/enumgen"
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
	if err := os.MkdirAll(filepath.Join(filepath.Dir(here), "enum"), 0755); err != nil {
		return err
	}

	if err := WriteCode(filepath.Join(filepath.Dir(here), "enum/size.go"), c, Size); err != nil {
		return err
	}
	if err := WriteCode(filepath.Join(filepath.Dir(here), "enum/order.go"), c, Order); err != nil {
		return err
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
