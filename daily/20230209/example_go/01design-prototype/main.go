package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os"

	"github.com/iancoleman/orderedmap"
	invopop "github.com/invopop/jsonschema"
	reflectshape "github.com/podhmo/reflect-shape"
)

type TestUser struct {
	ID string `json:"id"`
}

type Recorder struct {
	cfg       *reflectshape.Config
	reflector *invopop.Reflector

	seen           map[int]*Type
	order          []int
	OnMissingField func(rs *Type, name string) *Field
}

func NewRecorder() *Recorder {
	rec := &Recorder{
		cfg:       &reflectshape.Config{SkipComments: true},
		reflector: &invopop.Reflector{},
		seen:      map[int]*Type{},
	}
	rec.OnMissingField = rec.onMissingFieldDefault
	return rec
}

func (r *Recorder) onMissingFieldDefault(rs *Type, name string) *Field {
	panic(fmt.Sprintf("Field %s is missing in %v", name, rs.Shape))
}

func (r Recorder) MarshalJSON() ([]byte, error) {
	d := orderedmap.New()
	defs := orderedmap.New()
	d.Set("$schema", "https://json-schema.org/draft/2020-12/schema")
	d.Set("$defs", defs)

	seen := make(map[int]bool, len(r.order))
	ref := ""
	for _, id := range r.order {
		_, ok := seen[id]
		if ok {
			continue
		}
		seen[id] = true

		v := r.seen[id]

		schema := v.Schema
		switch len(schema.Definitions) {
		case 0:
			ref = v.Shape.Shape.FullName()
			defs.Set(ref, schema)
		case 1:
			for _, schema := range schema.Definitions {
				ref = v.Shape.Shape.FullName()
				defs.Set(ref, schema)
				break
			}
		default:
			panic("not supported") // TODO: implemented

		}
	}
	d.Set("$ref", "#/defs/"+ref)
	return json.Marshal(d)
}

func (r *Recorder) TypeOf(ob interface{}) *Type {
	shape := r.cfg.Extract(ob)
	id := shape.Number
	if v, ok := r.seen[id]; ok {
		return v
	}

	s := shape.Struct()
	src := s.Fields()

	schema := r.reflector.ReflectFromType(shape.Type)
	fields := make(map[string]*Field, src.Len())
	for _, f := range src {
		fieldname := f.Tag.Get("json") // TODO: omitempty
		if fieldname == "-" {
			continue
		}

		subschema := r.reflector.ReflectFromType(f.Shape.Type) // TODO: cache, TODO: parameters
		fields[f.Name] = &Field{Field: f, Schema: subschema}
		if schema.Properties == nil {
			schema.Properties = orderedmap.New()
		}
		schema.Properties.Set(fieldname, subschema)
	}
	v := &Type{
		recorder:  r,
		Shape:     s,
		src:       src,
		dstFields: fields,
		Schema:    schema,
	}
	// TODO: recursive
	r.seen[id] = v
	r.order = append(r.order, id)
	return v
}

type Type struct {
	Shape  *reflectshape.Struct
	Schema *Schema

	recorder *Recorder

	src       reflectshape.FieldList
	dstFields map[string]*Field
}

func (rs *Type) FieldByName(name string) *Field {
	if v, ok := rs.dstFields[name]; ok {
		return v
	}
	return rs.recorder.OnMissingField(rs, name)
}

type Field struct {
	*reflectshape.Field
	Schema *Schema
}

type Schema = invopop.Schema

func main() {
	rec := NewRecorder()
	{
		st := rec.TypeOf(&TestUser{})
		st.FieldByName("ID")
	}

	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(rec); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
