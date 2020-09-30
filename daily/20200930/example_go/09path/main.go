package main

import (
	"encoding/json"
	"m/shape"
	"os"
	"reflect"
	"strings"

	"github.com/getkin/kin-openapi/openapi3"
)

// TODO: reference schema
// TODO: validation for schema
// TODO: support path parameter
// TODO: support query, header, cookie with input data
// TODO: support function input
// TODO: extra information
// TODO: integration with net/http.Handler
// TODO: integration with fasthttp.Handler
// TODO: json tag inline,omitempty support
// TODO: schema required, unrequired support
// TODO: schema nullable support (?)

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
	Name string `json:"name"` // required
	Age  int    `json:"age"`
}

type ListInput struct {
	Limit int `openapi:"query"` // todo: see int field?
}

func ListPerson(in ListInput) []Person {
	return nil
}
func GetPerson() Person {
	return Person{}
}
func PutPerson(input PutPersonInput2) Person {
	return Person{}
}

type PutPersonInput struct {
	PersonID string `json:"personId" openapi:"path"`
	Person   Person `json:"person"` // treat as openapi:"json"
}
type PutPersonInput2 struct {
	PersonID string `json:"personId" openapi:"path"`
	Person          // inline
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
			oaType, ok := s.Tags[i].Lookup("openapi")
			if ok {
				switch strings.ToLower(oaType) {
				case "cookie", "header", "path", "query":
					// log.debug: skip this is not body's field
					continue
				}
			}

			// TODO: primitive
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
			case reflect.Struct, reflect.Ptr:
				f := t.Transform(v).(*openapi3.Schema) // xxx

				if !s.Metadata[i].Anonymous {
					schema.Properties[name] = &openapi3.SchemaRef{Value: f}
				} else { // embedded
					for subname, subf := range f.Properties {
						schema.Properties[subname] = subf
					}
				}

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

		// parameters
		if len(s.Params.Values) > 0 {
			// todo: support (ctx, ob)

			// scan body
			inob := s.Params.Values[0]
			schema := t.Transform(inob).(*openapi3.Schema) // xxx
			if len(schema.Properties) > 0 {
				// todo: required,content,description
				body := openapi3.NewRequestBody().
					WithJSONSchema(schema)
				op.RequestBody = &openapi3.RequestBodyRef{Value: body}
			}

			// scan other
			switch inob := inob.(type) {
			case shape.Struct:
				params := openapi3.NewParameters()
				for i, _ := range inob.Fields.Values {
					paramType, ok := inob.Tags[i].Lookup("openapi")
					if !ok {
						continue
					}
					// todo: required, type
					switch strings.ToLower(paramType) {
					case "json":
						continue
					case "path":
						params = append(params, &openapi3.ParameterRef{
							Value: openapi3.NewPathParameter(inob.FieldName(i)),
						})
					case "query":
						params = append(params, &openapi3.ParameterRef{
							Value: openapi3.NewQueryParameter(inob.FieldName(i)),
						})
					case "header":
						params = append(params, &openapi3.ParameterRef{
							Value: openapi3.NewHeaderParameter(inob.FieldName(i)),
						})
					case "cookie":
						params = append(params, &openapi3.ParameterRef{
							Value: openapi3.NewCookieParameter(inob.FieldName(i)),
						})
					default:
						panic(paramType)
					}
				}
				if len(params) > 0 {
					op.Parameters = params
				}
			default:
				panic(inob)
			}
		}

		// responses
		{
			// todo: support (ob, error)
			outob := s.Returns.Values[0]
			schema := t.Transform(outob).(*openapi3.Schema) // xxx
			op.Responses["200"] = &openapi3.ResponseRef{
				Value: openapi3.NewResponse().WithDescription("").WithJSONSchema(schema),
			}
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
		Emit(v.VisitSchema(PutPersonInput{}))
	}
	{
		Emit(v.VisitSchema(PutPersonInput2{}))
	}
	// {
	// 	op := v.VisitFunc(GetPerson)
	// 	doc.AddOperation("/people/{id}", "GET", op)
	// }
	{
		op := v.VisitFunc(PutPerson)
		doc.AddOperation("/people/{personId}", "PUT", op) // todo: validation
	}
	{
		op := v.VisitFunc(ListPerson)
		doc.AddOperation("/people", "GET", op)
	}
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	enc.Encode(doc)
}
