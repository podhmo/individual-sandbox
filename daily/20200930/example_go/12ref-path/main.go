package main

import (
	"encoding/json"
	"fmt"
	"m/shape"
	"os"
	"reflect"
	"strings"
	"time"

	"github.com/getkin/kin-openapi/openapi3"
)

// TODO: validation for schema
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

func Emit(ob interface{}) error {
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	return enc.Encode(ob)
}

// not visitor pattern
type Visitor struct {
	*Transformer
	Doc        *openapi3.Swagger
	Schemas    map[reflect.Type]*openapi3.Schema
	Operations map[reflect.Type]*openapi3.Operation
}

func NewVisitor(resolver Resolver) *Visitor {
	return &Visitor{
		Transformer: (&Transformer{
			cache:            map[reflect.Type]interface{}{},
			interceptFuncMap: map[reflect.Type]func(shape.Shape) *openapi3.Schema{},
			Resolver:         resolver,
		}).Builtin(),
		Schemas:    map[reflect.Type]*openapi3.Schema{},
		Operations: map[reflect.Type]*openapi3.Operation{},
	}
}

func (v *Visitor) VisitType(ob interface{}, modifiers ...func(*openapi3.Schema)) *openapi3.SchemaRef {
	in := shape.Extract(ob)
	out := v.Transform(in).(*openapi3.Schema)
	for _, m := range modifiers {
		m(out)
	}
	v.Schemas[in.GetReflectType()] = out
	return v.ResolveSchema(out, in)
}
func (v *Visitor) VisitFunc(ob interface{}, modifiers ...func(*openapi3.Operation)) *openapi3.Operation {
	// TODO: use schema ref?
	in := shape.Extract(ob)
	out := v.Transform(in).(*openapi3.Operation)
	for _, m := range modifiers {
		m(out)
	}
	v.Operations[in.GetReflectType()] = out
	return out
}

type Transformer struct {
	cache    map[reflect.Type]interface{}
	CacheHit int

	interceptFuncMap map[reflect.Type]func(shape.Shape) *openapi3.Schema
	Resolver
}

func (t *Transformer) Builtin() *Transformer {
	// todo: handling required?
	{
		var z []byte
		t.interceptFuncMap[reflect.ValueOf(z).Type()] = func(s shape.Shape) *openapi3.Schema {
			return openapi3.NewBytesSchema()
		}
	}
	{
		var z time.Time
		t.interceptFuncMap[reflect.ValueOf(z).Type()] = func(s shape.Shape) *openapi3.Schema {
			return openapi3.NewDateTimeSchema()
		}
	}
	return t
}

type Resolver interface {
	ResolveSchema(v *openapi3.Schema, s shape.Shape) *openapi3.SchemaRef
	ResolveParameter(v *openapi3.Parameter, s shape.Shape) *openapi3.ParameterRef
	ResolveRequestBody(v *openapi3.RequestBody, s shape.Shape) *openapi3.RequestBodyRef
	ResolveResponse(v *openapi3.Response, s shape.Shape) *openapi3.ResponseRef
}

type Binder interface {
	Bind(doc *openapi3.Swagger)
}

type NoRefResolver struct{}

func (r *NoRefResolver) ResolveSchema(v *openapi3.Schema, s shape.Shape) *openapi3.SchemaRef {
	return &openapi3.SchemaRef{Value: v}
}
func (r *NoRefResolver) ResolveParameter(v *openapi3.Parameter, s shape.Shape) *openapi3.ParameterRef {
	return &openapi3.ParameterRef{Value: v}
}
func (r *NoRefResolver) ResolveRequestBody(v *openapi3.RequestBody, s shape.Shape) *openapi3.RequestBodyRef {
	return &openapi3.RequestBodyRef{Value: v}
}
func (r *NoRefResolver) ResolveResponse(v *openapi3.Response, s shape.Shape) *openapi3.ResponseRef {
	return &openapi3.ResponseRef{Value: v}
}
func (r *NoRefResolver) Bind(doc *openapi3.Swagger) {
}

type UseRefResolver struct {
	Schemas []*openapi3.SchemaRef
}

func (r *UseRefResolver) ResolveSchema(v *openapi3.Schema, s shape.Shape) *openapi3.SchemaRef {
	switch s := s.(type) {
	case shape.Primitive, shape.Container:
		return &openapi3.SchemaRef{Value: v}
	default:
		ref := fmt.Sprintf("#/components/schemas/%s", s.GetName())
		r.Schemas = append(r.Schemas, &openapi3.SchemaRef{Ref: ref, Value: v})
		return &openapi3.SchemaRef{Ref: ref}
	}
}
func (r *UseRefResolver) ResolveParameter(v *openapi3.Parameter, s shape.Shape) *openapi3.ParameterRef {
	return &openapi3.ParameterRef{Value: v}
}
func (r *UseRefResolver) ResolveRequestBody(v *openapi3.RequestBody, s shape.Shape) *openapi3.RequestBodyRef {
	return &openapi3.RequestBodyRef{Value: v}
}
func (r *UseRefResolver) ResolveResponse(v *openapi3.Response, s shape.Shape) *openapi3.ResponseRef {
	return &openapi3.ResponseRef{Value: v}
}
func (r *UseRefResolver) Bind(doc *openapi3.Swagger) {
	if len(r.Schemas) == 0 {
		return
	}

	if doc.Components.Schemas == nil {
		doc.Components.Schemas = map[string]*openapi3.SchemaRef{}
	}
	for _, ref := range r.Schemas {
		ref := ref
		path := ref.Ref
		ref.Ref = ""
		doc.Components.Schemas[path] = ref
	}
}

func (t *Transformer) Transform(s shape.Shape) interface{} { // *Operation | *Schema | *Response
	rt := s.GetReflectType()
	if retval, ok := t.cache[rt]; ok {
		t.CacheHit++
		return retval
	}

	// e.g. for time.Time as {"type": "string", "format": "date-time"}
	if intercept, ok := t.interceptFuncMap[rt]; ok {
		retval := intercept(s)
		t.cache[rt] = retval
		return retval
	}

	switch s := s.(type) {
	case shape.Primitive:
		switch s.GetReflectKind() {
		case reflect.Bool:
			return openapi3.NewBoolSchema()
		case reflect.String:
			return openapi3.NewStringSchema()
		case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
			// Todo: use NewInt64Schema?
			return openapi3.NewIntegerSchema()
		case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64: // Uintptr
			return openapi3.NewIntegerSchema()
		case reflect.Float32, reflect.Float64:
			return openapi3.NewFloat64Schema()
		default:
			notImplementedYet(s)
		}
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

			name := s.FieldName(i)

			// todo: support required

			switch v.GetReflectKind() {
			case reflect.Struct:
				f := t.Transform(v).(*openapi3.Schema) // xxx

				if !s.Metadata[i].Anonymous {
					schema.Properties[name] = t.ResolveSchema(f, v)
				} else { // embedded
					for subname, subf := range f.Properties {
						schema.Properties[subname] = subf
					}
				}
			case reflect.Func, reflect.Chan:
				continue
			default:
				f := t.Transform(v).(*openapi3.Schema) // xxx
				schema.Properties[name] = t.ResolveSchema(f, v)
			}
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
					WithJSONSchemaRef(t.ResolveSchema(schema, inob))
				op.RequestBody = t.ResolveRequestBody(body, inob)
			}

			// scan other
			switch inob := inob.(type) {
			case shape.Struct:
				params := openapi3.NewParameters()
				for i, v := range inob.Fields.Values {
					paramType, ok := inob.Tags[i].Lookup("openapi")
					if !ok {
						continue
					}
					// todo: required, type
					switch strings.ToLower(paramType) {
					case "json":
						continue
					case "path":
						params = append(params, t.ResolveParameter(
							openapi3.NewPathParameter(inob.FieldName(i)), v),
						)
					case "query":
						params = append(params, t.ResolveParameter(
							openapi3.NewQueryParameter(inob.FieldName(i)), v),
						)
					case "header":
						params = append(params, t.ResolveParameter(
							openapi3.NewHeaderParameter(inob.FieldName(i)), v),
						)
					case "cookie":
						params = append(params, t.ResolveParameter(
							openapi3.NewCookieParameter(inob.FieldName(i)), v),
						)
					default:
						panic(paramType)
					}
				}
				if len(params) > 0 {
					op.Parameters = params
				}
			default:
				fmt.Println("only struct")
				panic(inob)
			}
		}

		// responses
		{
			// todo: support (ob, error)
			outob := s.Returns.Values[0]
			schema := t.Transform(outob).(*openapi3.Schema) // xxx
			op.Responses["200"] = t.ResolveResponse(
				openapi3.NewResponse().WithDescription("").WithJSONSchemaRef(
					t.ResolveSchema(schema, outob),
				),
				s.Returns.Values[0],
			)
		}
		t.cache[rt] = op
		return op
	case shape.Container:
		// container is map,slice,array
		switch s.GetReflectKind() {
		case reflect.Slice:
			schema := openapi3.NewArraySchema()
			inner := t.Transform(s.Args[0]).(*openapi3.Schema)
			schema.Items = t.ResolveSchema(inner, s.Args[0])
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

type GetInput struct {
	PersonID string `json:"personId" openapi:"path"`
}

func GetPerson(in GetInput) Person {
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

func main() {
	// r := &NoRefResolver{}
	r := &UseRefResolver{}
	v := NewVisitor(r)
	doc := &openapi3.Swagger{}

	v.VisitType(Person{}, func(s *openapi3.Schema) {
		s.Description = "someone using it"
	})
	v.VisitFunc(ListPerson, func(op *openapi3.Operation) {
		doc.AddOperation("/people", "GET", op)
	})
	{
		op := v.VisitFunc(GetPerson)
		doc.AddOperation("/people/{personId}", "GET", op)
	}

	r.Bind(doc)

	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	enc.Encode(doc)
}
