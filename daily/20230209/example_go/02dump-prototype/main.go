package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
	"strings"
	"time"

	"github.com/iancoleman/orderedmap"
	reflectshape "github.com/podhmo/reflect-shape"
)

// ----------------------------------------

type Recorder struct {
	cfg         *reflectshape.Config
	definitions *orderedmap.OrderedMap

	seen           map[int]*Type
	order          []int
	OnMissingField func(rs *Type, name string) *Field
}

func NewRecorder() *Recorder {
	rec := &Recorder{
		cfg:         &reflectshape.Config{SkipComments: false},
		definitions: orderedmap.New(),
		seen:        map[int]*Type{},
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
	d.Set("$schema", Version)
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
		ref = v.Shape.Shape.FullName()
		defs.Set(v.Shape.Shape.FullName(), v.Node)
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

	node := &Node{shape: s.Shape}
	fields := make(map[string]*Field, src.Len())
	for _, f := range src {
		fieldname := f.Name
		if jsonTagString, ok := f.Tag.Lookup("json"); ok {
			jsonTags := strings.Split(jsonTagString, ",")
			fieldname = jsonTags[0]
		}
		if fieldname == "-" {
			continue
		}

		subNode := &Node{shape: f.Shape} // TODO: cache, TODO: parameters
		fields[f.Name] = &Field{Field: f, Node: subNode}
		if node.Properties == nil {
			node.Properties = orderedmap.New()
		}
		if f.Doc != "" {
			subNode.Description = f.Doc
		}
		node.Properties.Set(fieldname, subNode)
	}
	v := &Type{
		recorder:  r,
		Shape:     s,
		src:       src,
		dstFields: fields,
		Node:      node,
	}
	// TODO: recursive
	r.seen[id] = v
	r.order = append(r.order, id)
	return v
}

type Type struct {
	Shape *reflectshape.Struct
	Node  *Node

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
	Node *Node
}

// func (f *Field) Title(v string) *Field {
// 	f.Node.Title = v
// 	return f
// }
// func (f *Field) Description(v string) *Field {
// 	f.Node.Description = v
// 	return f
// }
// func (f *Field) Example(vs ...interface{}) *Field {
// 	f.Node.Examples = append(f.Node.Examples, vs...)
// 	return f
// }
// func (f *Field) Default(v interface{}) *Field {
// 	f.Node.Default = v
// 	return f
// }
// func (f *Field) Enum(v ...interface{}) *Field {
// 	f.Node.Enum = v
// 	return f
// }

// --
var Version = "https://json-Node.org/draft/2020-12/Node"

type Node struct {
	shape *reflectshape.Shape

	Description string                 `json:"description,omitempty"`
	Properties  *orderedmap.OrderedMap `json:"properties,omitempty"`
}

// ----------------------------------------

type TestUser struct {
	ID      string                 `json:"id"`
	Name    string                 `json:"name"` // The name of a friend
	Friends []int                  `json:"friends,omitempty"`
	Tags    map[string]interface{} `json:"tags,omitempty" jsonNode_extras:"a=b,foo=bar,foo=bar1"` // TODO: dynamic types (a,foo)

	BirthDate   time.Time `json:"birth_date,omitempty" jsonNode:"oneof_required=date"` // TODO: oneof_required
	YearOfBirth string    `json:"year_of_birth,omitempty" jsonNode:"oneof_required=year"`

	Metadata interface{} `json:"metadata,omitempty" jsonNode:"oneof_type=string;array"` // oneOf

	FavColor string `json:"fav_color,omitempty"`
}

func main() {
	rec := NewRecorder()

	{
		st := rec.TypeOf(&TestUser{})
		_ = st
		// st.FieldByName("Name").Title("the name").Example("joe").Example("lucy").Default("alex")
		// st.FieldByName("Friends").Description("The list of IDs, omitted when empty")
		// st.FieldByName("FavColor").Enum("red", "green", "blue")
	}

	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(rec); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
