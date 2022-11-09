package main

import (
	"bytes"
	"fmt"
	"go/format"
	"io"
	"os"
	"strconv"
	"strings"
)

type Kind string

const (
	KindPrimitive Kind = "p"
	KindContainer Kind = "c"
	KindObject    Kind = "o"
)

type PrimitiveType struct {
	Underlying *PrimitiveType
}

func (t PrimitiveType) Kind() Kind {
	return KindPrimitive
}

type ContainerType struct {
	Args []Type
}

func (c ContainerType) Kind() Kind {
	return KindContainer
}

// TODO: embeded

type StructType struct {
	Fields []Field
}

func (s StructType) Kind() Kind {
	return KindObject
}

type Type interface {
	Kind() Kind
}

type Named struct {
	Name string
	Type Type
}

func (t Named) Kind() Kind {
	return t.Type.Kind()
}

type Field struct {
	Name string
	Type Type
	Tags []string
}

func (f Field) Tag(name, value string) Field {
	new := f
	new.Tags = append(new.Tags[:], name, value)
	return new
}

// TODO: new type
// TODO: enum

var (
	StringType = Named{Name: "string", Type: PrimitiveType{}}
	IntType    = Named{Name: "int", Type: PrimitiveType{}}
)

func String(name string) Field {
	return Field{
		Name: name,
		Type: StringType,
	}
}
func Int(name string) Field {
	return Field{
		Name: name,
		Type: IntType,
	}
}

func Object(name string, fields ...Field) Type {
	return Named{Name: name, Type: StructType{Fields: fields}}
}
func List(name string, typ Type) Type {
	return Named{Name: name, Type: ContainerType{Args: []Type{typ}}}
}

// TODO: Product?
func typeName(typ Type) string {
	if t, ok := typ.(Named); ok {
		return t.Name
	}
	return "<?>"
}
func ToGoCode(w io.Writer, typ Type) {
	switch typ.Kind() {
	case KindObject:
		fmt.Fprintf(w, "type %s struct {\n", typeName(typ))
		for _, f := range typ.(Named).Type.(StructType).Fields {
			// TODO: goify
			typename := typeName(f.Type)
			if t, ok := f.Type.(interface{ GoTypeName() string }); ok {
				typename = t.GoTypeName()
			}

			if len(f.Tags) == 0 {
				fmt.Fprintf(w, "\t%s %s\n", f.Name, typename)
			} else {
				tags := make([]string, 0, len(f.Tags)/2)
				for i := 0; i < len(f.Tags); i += 2 {
					tags = append(tags, fmt.Sprintf("%s:%s", f.Tags[i], strconv.Quote(f.Tags[i+1])))
				}
				fmt.Fprintf(w, "\t%s %s `%s`\n", f.Name, typename, strings.Join(tags, ", "))
			}
		}
		fmt.Fprintf(w, "}\n")
	default:
		panic(fmt.Sprintf("unexpected kind: %s", typ.Kind()))
	}
}

func ToJSONSchema(w io.Writer, typ Type) {
	switch typ.Kind() {
	case KindObject:
		fmt.Fprintf(w, "{\n")
		fmt.Fprintf(w, "  \"title\": %q,\n", typeName(typ))
		fmt.Fprintf(w, "  \"type\": \"object\",\n")
		fmt.Fprintf(w, "  \"properties\": {\n")
		for _, f := range typ.(Named).Type.(StructType).Fields {
			name := f.Name
			typename := typeName(f.Type)
			if t, ok := f.Type.(interface{ JSONTypeName() string }); ok {
				typename = t.JSONTypeName()
			}

			for i := 0; i < len(f.Tags); i += 2 {
				if f.Tags[i] == "json" {
					name = f.Tags[i+1]
					break
				}
			}
			fmt.Fprintf(w, "    %q: {\"type\": %q},\n", name, typename)
		}
		fmt.Fprintf(w, "  }\n")
		// TODO: required
		fmt.Fprintf(w, "}\n")
	default:
		panic(fmt.Sprintf("unexpected kind: %s", typ.Kind()))
	}
}

func main() {
	product := Object("Product",
		Int("ID").Tag("json", "productId"),        // .Required().Regexp(`^[A-Z][A-Za-z_]+$`),
		String("Name").Tag("json", "productName"), // .Requored()
	)
	// productList := List("ProductList", product)
	// productMap := Map("ProductMap", String, product)

	// {
	// 	"$schema": "https://json-schema.org/draft/2020-12/schema",
	// 	"$id": "https://example.com/product.schema.json",
	// 	"title": "Product",
	// 	"description": "A product from Acme's catalog",
	// 	"type": "object",
	// 	"properties": {
	// 	  "productId": {
	// 		"description": "The unique identifier for a product",
	// 		"type": "integer"
	// 	  },
	// 	  "productName": {
	// 		"description": "Name of the product",
	// 		"type": "string"
	// 	  }
	// 	},
	// 	"required": [ "productId", "productName" ]
	//   }

	{
		buf := new(bytes.Buffer)
		ToGoCode(buf, product)
		b, err := format.Source(buf.Bytes())
		if err != nil {
			panic(err)
		}
		os.Stdout.Write(b) // nolint
	}
	{
		buf := new(bytes.Buffer)
		ToJSONSchema(buf, product)
		io.Copy(os.Stdout, buf)
	}
}

// {
//   "productId": 1,
//   "productName": "A green door",
//   "price": 12.50,
//   "tags": [ "home", "green" ]
// }
