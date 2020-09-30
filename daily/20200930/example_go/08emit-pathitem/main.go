package main

import (
	"encoding/json"
	"m/shape"
	"os"
	"reflect"

	"github.com/getkin/kin-openapi/openapi3"
)

// type Kind uint

// const (
// 	Invalid Kind = iota
// 	Bool
// 	Int
// 	Int8
// 	Int16
// 	Int32
// 	Int64
// 	Uint
// 	Uint8
// 	Uint16
// 	Uint32
// 	Uint64
// 	Uintptr
// 	Float32
// 	Float64
// 	Complex64
// 	Complex128
// 	Array
// 	Chan
// 	Func
// 	Interface
// 	Map
// 	Ptr
// 	Slice
// 	String
// 	Struct
// 	UnsafePointer
// )

type Person struct {
	Title string `json:"title"` // required
	Age   int    `json:"age"`
}

func ListPerson() []Person {
	return nil
}
func GetPerson() Person {
	return Person{}
}

func Emit(ob interface{}) error {
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	return enc.Encode(ob)
}

// not visitor pattern
type Visitor struct {
	*Transformer
	Doc        *openapi3.Swagger
	SchemaRefs map[reflect.Type]*openapi3.SchemaRef
	Operations map[reflect.Type]*openapi3.Operation
}

func NewVisitor() *Visitor {
	return &Visitor{
		Transformer: &Transformer{cache: map[reflect.Type]interface{}{}},
		SchemaRefs:  map[reflect.Type]*openapi3.SchemaRef{},
		Operations:  map[reflect.Type]*openapi3.Operation{},
	}
}

func (v *Visitor) VisitSchema(ob interface{}) *openapi3.SchemaRef {
	in := shape.Extract(ob)
	out := v.Transform(in).(*openapi3.Schema)
	retval := &openapi3.SchemaRef{Value: out}
	v.SchemaRefs[in.GetReflectType()] = retval
	return retval
}
func (v *Visitor) VisitFunc(ob interface{}) *openapi3.Operation {
	// TODO: use schema ref?
	in := shape.Extract(ob)
	out := v.Transform(in).(*openapi3.Operation)
	retval := out
	v.Operations[in.GetReflectType()] = retval
	return retval
}

type Transformer struct {
	cache    map[reflect.Type]interface{}
	CacheHit int
}

// TODO: cache
func (t *Transformer) Transform(s shape.Shape) interface{} {
	rt := s.GetReflectType()
	if retval, ok := t.cache[rt]; ok {
		t.CacheHit++
		return retval
	}

	switch s := s.(type) {
	case shape.Struct:
		// return *openapi3.Schema (TODO: schema ref?)

		schema := openapi3.NewObjectSchema()
		for i, v := range s.Fields.Values {
			// todo: json tag
			name := s.FieldName(i)
			switch v.GetReflectKind() {
			case reflect.String:
				f := openapi3.NewStringSchema()
				schema.Properties[name] = &openapi3.SchemaRef{Value: f}
			case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
				f := openapi3.NewIntegerSchema()
				schema.Properties[name] = &openapi3.SchemaRef{Value: f}
			case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64: // Uintptr
				f := openapi3.NewIntegerSchema()
				schema.Properties[name] = &openapi3.SchemaRef{Value: f}
			default:
				notImplementedYet(v)
			}

			// support required
		}
		t.cache[rt] = schema
		return schema
	case shape.Function:
		// return *openapi.Operation
		// as interactor (TODO: meta tag? for specific usecase)

		op := openapi3.NewOperation()
		op.OperationID = s.GetFullName()
		op.Responses = openapi3.NewResponses()

		// todo: support (ob, error)
		retob := s.Returns.Values[0]
		schema := t.Transform(retob).(*openapi3.Schema) // xxx
		op.Responses["200"] = &openapi3.ResponseRef{
			Value: openapi3.NewResponse().WithDescription("").WithJSONSchema(schema),
		}
		t.cache[rt] = op
		return op
	case shape.Container:
		// container is map,slice,array
		switch s.GetReflectKind() {
		case reflect.Slice:
			schema := openapi3.NewArraySchema()
			inner := t.Transform(s.Args[0]).(*openapi3.Schema)
			schema.Items = &openapi3.SchemaRef{Value: inner}
			t.cache[rt] = schema
			return schema
		default:
			notImplementedYet(s)
		}
		notImplementedYet(s)
	default:
		notImplementedYet(s)
	}
	panic("never")
}

func notImplementedYet(ob interface{}) {
	panic(ob)
}

func main() {
	doc := &openapi3.Swagger{}
	v := NewVisitor()
	{
		op := v.VisitFunc(GetPerson)
		doc.AddOperation("/people/{id}", "GET", op)
	}
	{
		op := v.VisitFunc(ListPerson)
		doc.AddOperation("/people", "GET", op)
	}
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	enc.Encode(doc)
}
