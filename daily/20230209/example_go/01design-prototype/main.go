package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net"
	"net/url"
	"os"
	"reflect"
	"strings"
	"time"

	"github.com/iancoleman/orderedmap"
	reflectshape "github.com/podhmo/reflect-shape"
)

type Definitions *orderedmap.OrderedMap
type ID string

// reflector is minimum version of invopop/jsoschema's Reflector
type reflector struct {
	AllowAdditionalProperties bool
	IgnoredTypes              []interface{}
	Definitions               Definitions
}

// Available Go defined types for JSON Schema Validation.
// RFC draft-wright-json-schema-validation-00, section 7.3
var (
	timeType = reflect.TypeOf(time.Time{}) // date-time RFC section 7.3.1
	ipType   = reflect.TypeOf(net.IP{})    // ipv4 and ipv6 RFC section 7.3.4, 7.3.5
	uriType  = reflect.TypeOf(url.URL{})   // uri RFC section 7.3.6
)

// Byte slices will be encoded as base64
var byteSliceType = reflect.TypeOf([]byte(nil))

// Except for json.RawMessage
var rawMessageType = reflect.TypeOf(json.RawMessage{})

// Go code generated from protobuf enum types should fulfil this interface.
type protoEnum interface {
	EnumDescriptor() ([]byte, []int)
}

var protoEnumType = reflect.TypeOf((*protoEnum)(nil)).Elem()

func (r *reflector) ReflectFromType(t reflect.Type) *Schema {
	return r.reflectTypeToSchema(r.Definitions, t)
}

func (r *reflector) reflectTypeToSchema(definitions Definitions, t reflect.Type) *Schema {
	// Prepare a base to which details can be added
	st := new(Schema)

	// jsonpb will marshal protobuf enum options as either strings or integers.
	// It will unmarshal either.
	if t.Implements(protoEnumType) {
		st.OneOf = []*Schema{
			{Type: "string"},
			{Type: "integer"},
		}
		return st
	}

	// Defined format types for JSON Schema Validation
	// RFC draft-wright-json-schema-validation-00, section 7.3
	// TODO email RFC section 7.3.2, hostname RFC section 7.3.3, uriref RFC section 7.3.7
	if t == ipType {
		// TODO differentiate ipv4 and ipv6 RFC section 7.3.4, 7.3.5
		st.Type = "string"
		st.Format = "ipv4"
		return st
	}

	switch t.Kind() {
	case reflect.Struct:
		r.reflectStruct(definitions, t, st)

	case reflect.Slice, reflect.Array:
		r.reflectSliceOrArray(definitions, t, st)

	case reflect.Map:
		r.reflectMap(definitions, t, st)

	case reflect.Interface:
		// empty

	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64,
		reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		st.Type = "integer"

	case reflect.Float32, reflect.Float64:
		st.Type = "number"

	case reflect.Bool:
		st.Type = "boolean"

	case reflect.String:
		st.Type = "string"

	default:
		panic("unsupported type " + t.String())
	}

	return st
}

func (r *reflector) reflectSliceOrArray(definitions Definitions, t reflect.Type, st *Schema) {
	if t == rawMessageType {
		return
	}

	if t.Kind() == reflect.Array {
		st.MinItems = t.Len()
		st.MaxItems = st.MinItems
	}
	if t.Kind() == reflect.Slice && t.Elem() == byteSliceType.Elem() {
		st.Type = "string"
		// NOTE: ContentMediaType is not set here
		st.ContentEncoding = "base64"
	} else {
		st.Type = "array"
		st.Items = r.reflectTypeToSchema(definitions, t.Elem())
	}
}

func (r *reflector) reflectMap(definitions Definitions, t reflect.Type, st *Schema) {
	st.Type = "object"

	switch t.Key().Kind() {
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		st.PatternProperties = map[string]*Schema{
			"^[0-9]+$": r.reflectTypeToSchema(definitions, t.Elem()),
		}
		st.AdditionalProperties = FalseSchema
		return
	}
	if t.Elem().Kind() != reflect.Interface {
		st.PatternProperties = map[string]*Schema{
			".*": r.reflectTypeToSchema(definitions, t.Elem()),
		}
	}
}

// Reflects a struct to a JSON Schema type.
func (r *reflector) reflectStruct(definitions Definitions, t reflect.Type, s *Schema) {
	// Handle special types
	switch t {
	case timeType: // date-time RFC section 7.3.1
		s.Type = "string"
		s.Format = "date-time"
		return
	case uriType: // uri RFC section 7.3.6
		s.Type = "string"
		s.Format = "uri"
		return
	}

	s.Type = "object"
	s.Properties = orderedmap.New()
	if !r.AllowAdditionalProperties {
		s.AdditionalProperties = FalseSchema
	}

	ignored := false
	for _, it := range r.IgnoredTypes {
		if reflect.TypeOf(it) == t {
			ignored = true
			break
		}
	}
	if !ignored {
		r.reflectStructFields(s, definitions, t)
	}
}

func (r *reflector) reflectStructFields(st *Schema, definitions Definitions, t reflect.Type) {
	if t.Kind() == reflect.Ptr {
		t = t.Elem()
	}
	if t.Kind() != reflect.Struct {
		return
	}

	handleField := func(f reflect.StructField) {
		name, shouldEmbed, required, nullable := r.reflectFieldName(f)
		// if anonymous and exported type should be processed recursively
		// current type should inherit properties of anonymous one
		if name == "" {
			if shouldEmbed {
				r.reflectStructFields(st, definitions, f.Type)
			}
			return
		}

		property := r.reflectTypeToSchema(definitions, f.Type)

		if nullable {
			property = &Schema{
				OneOf: []*Schema{
					property,
					{
						Type: "null",
					},
				},
			}
		}

		st.Properties.Set(name, property)
		if required {
			st.Required = appendUniqueString(st.Required, name)
		}
	}

	for i := 0; i < t.NumField(); i++ {
		f := t.Field(i)
		handleField(f)
	}
}

func (r *reflector) reflectFieldName(f reflect.StructField) (string, bool, bool, bool) {
	jsonTagString, _ := f.Tag.Lookup("json")
	jsonTags := strings.Split(jsonTagString, ",")
	required := true
	for _, tag := range jsonTags {
		if tag == "omitempty" {
			required = false
			break
		}
	}
	if f.Anonymous && jsonTags[0] == "" {
		// As per JSON Marshal rules, anonymous structs are inherited
		if f.Type.Kind() == reflect.Struct {
			return "", true, false, false
		}

		// As per JSON Marshal rules, anonymous pointer to structs are inherited
		if f.Type.Kind() == reflect.Ptr && f.Type.Elem().Kind() == reflect.Struct {
			return "", true, false, false
		}
	}

	// Try to determine the name from the different combos
	name := f.Name
	if jsonTags[0] != "" {
		name = jsonTags[0]
	}
	if !f.Anonymous && f.PkgPath != "" {
		// field not anonymous and not export has no export name
		name = ""
	}

	return name, false, required, false
}

func appendUniqueString(base []string, value string) []string {
	for _, v := range base {
		if v == value {
			return base
		}
	}
	return append(base, value)
}

// ----------------------------------------

type Recorder struct {
	cfg       *reflectshape.Config
	reflector *reflector

	seen           map[int]*Type
	order          []int
	OnMissingField func(rs *Type, name string) *Field
}

func NewRecorder() *Recorder {
	rec := &Recorder{
		cfg:       &reflectshape.Config{SkipComments: false},
		reflector: &reflector{Definitions: orderedmap.New()},
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
		defs.Set(v.Shape.Shape.FullName(), v.Schema)
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
		fieldname := f.Name
		if jsonTagString, ok := f.Tag.Lookup("json"); ok {
			jsonTags := strings.Split(jsonTagString, ",")
			fieldname = jsonTags[0]
		}
		if fieldname == "-" {
			continue
		}

		subschema := r.reflector.ReflectFromType(f.Shape.Type) // TODO: cache, TODO: parameters
		fields[f.Name] = &Field{Field: f, Schema: subschema}
		if schema.Properties == nil {
			schema.Properties = orderedmap.New()
		}
		if f.Doc != "" {
			subschema.Description = f.Doc
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

func (f *Field) Title(v string) *Field {
	f.Schema.Title = v
	return f
}
func (f *Field) Description(v string) *Field {
	f.Schema.Description = v
	return f
}
func (f *Field) Example(vs ...interface{}) *Field {
	f.Schema.Examples = append(f.Schema.Examples, vs...)
	return f
}
func (f *Field) Default(v interface{}) *Field {
	f.Schema.Default = v
	return f
}
func (f *Field) Enum(v ...interface{}) *Field {
	f.Schema.Enum = v
	return f
}

// --
// Version is the JSON Schema version.
var Version = "https://json-schema.org/draft/2020-12/schema"

// Schema represents a JSON Schema object type.
// RFC draft-bhutton-json-schema-00 section 4.3
type Schema struct {
	// RFC draft-bhutton-json-schema-00
	Version     string      `json:"$schema,omitempty"`     // section 8.1.1
	ID          ID          `json:"$id,omitempty"`         // section 8.2.1
	Anchor      string      `json:"$anchor,omitempty"`     // section 8.2.2
	Ref         string      `json:"$ref,omitempty"`        // section 8.2.3.1
	DynamicRef  string      `json:"$dynamicRef,omitempty"` // section 8.2.3.2
	Definitions Definitions `json:"$defs,omitempty"`       // section 8.2.4
	Comments    string      `json:"$comment,omitempty"`    // section 8.3
	// RFC draft-bhutton-json-schema-00 section 10.2.1 (Sub-schemas with logic)
	AllOf []*Schema `json:"allOf,omitempty"` // section 10.2.1.1
	AnyOf []*Schema `json:"anyOf,omitempty"` // section 10.2.1.2
	OneOf []*Schema `json:"oneOf,omitempty"` // section 10.2.1.3
	Not   *Schema   `json:"not,omitempty"`   // section 10.2.1.4
	// RFC draft-bhutton-json-schema-00 section 10.2.2 (Apply sub-schemas conditionally)
	If               *Schema            `json:"if,omitempty"`               // section 10.2.2.1
	Then             *Schema            `json:"then,omitempty"`             // section 10.2.2.2
	Else             *Schema            `json:"else,omitempty"`             // section 10.2.2.3
	DependentSchemas map[string]*Schema `json:"dependentSchemas,omitempty"` // section 10.2.2.4
	// RFC draft-bhutton-json-schema-00 section 10.3.1 (arrays)
	PrefixItems []*Schema `json:"prefixItems,omitempty"` // section 10.3.1.1
	Items       *Schema   `json:"items,omitempty"`       // section 10.3.1.2  (replaces additionalItems)
	Contains    *Schema   `json:"contains,omitempty"`    // section 10.3.1.3
	// RFC draft-bhutton-json-schema-00 section 10.3.2 (sub-schemas)
	Properties           *orderedmap.OrderedMap `json:"properties,omitempty"`           // section 10.3.2.1
	PatternProperties    map[string]*Schema     `json:"patternProperties,omitempty"`    // section 10.3.2.2
	AdditionalProperties *Schema                `json:"additionalProperties,omitempty"` // section 10.3.2.3
	PropertyNames        *Schema                `json:"propertyNames,omitempty"`        // section 10.3.2.4
	// RFC draft-bhutton-json-schema-validation-00, section 6
	Type              string              `json:"type,omitempty"`              // section 6.1.1
	Enum              []interface{}       `json:"enum,omitempty"`              // section 6.1.2
	Const             interface{}         `json:"const,omitempty"`             // section 6.1.3
	MultipleOf        int                 `json:"multipleOf,omitempty"`        // section 6.2.1
	Maximum           int                 `json:"maximum,omitempty"`           // section 6.2.2
	ExclusiveMaximum  bool                `json:"exclusiveMaximum,omitempty"`  // section 6.2.3
	Minimum           int                 `json:"minimum,omitempty"`           // section 6.2.4
	ExclusiveMinimum  bool                `json:"exclusiveMinimum,omitempty"`  // section 6.2.5
	MaxLength         int                 `json:"maxLength,omitempty"`         // section 6.3.1
	MinLength         int                 `json:"minLength,omitempty"`         // section 6.3.2
	Pattern           string              `json:"pattern,omitempty"`           // section 6.3.3
	MaxItems          int                 `json:"maxItems,omitempty"`          // section 6.4.1
	MinItems          int                 `json:"minItems,omitempty"`          // section 6.4.2
	UniqueItems       bool                `json:"uniqueItems,omitempty"`       // section 6.4.3
	MaxContains       uint                `json:"maxContains,omitempty"`       // section 6.4.4
	MinContains       uint                `json:"minContains,omitempty"`       // section 6.4.5
	MaxProperties     int                 `json:"maxProperties,omitempty"`     // section 6.5.1
	MinProperties     int                 `json:"minProperties,omitempty"`     // section 6.5.2
	Required          []string            `json:"required,omitempty"`          // section 6.5.3
	DependentRequired map[string][]string `json:"dependentRequired,omitempty"` // section 6.5.4
	// RFC draft-bhutton-json-schema-validation-00, section 7
	Format string `json:"format,omitempty"`
	// RFC draft-bhutton-json-schema-validation-00, section 8
	ContentEncoding  string  `json:"contentEncoding,omitempty"`  // section 8.3
	ContentMediaType string  `json:"contentMediaType,omitempty"` // section 8.4
	ContentSchema    *Schema `json:"contentSchema,omitempty"`    // section 8.5
	// RFC draft-bhutton-json-schema-validation-00, section 9
	Title       string        `json:"title,omitempty"`       // section 9.1
	Description string        `json:"description,omitempty"` // section 9.1
	Default     interface{}   `json:"default,omitempty"`     // section 9.2
	Deprecated  bool          `json:"deprecated,omitempty"`  // section 9.3
	ReadOnly    bool          `json:"readOnly,omitempty"`    // section 9.4
	WriteOnly   bool          `json:"writeOnly,omitempty"`   // section 9.4
	Examples    []interface{} `json:"examples,omitempty"`    // section 9.5

	Extras map[string]interface{} `json:"-"`

	// Special boolean representation of the Schema - section 4.3.2
	boolean *bool
}

var (
	// TrueSchema defines a schema with a true value
	TrueSchema = &Schema{boolean: &[]bool{true}[0]}
	// FalseSchema defines a schema with a false value
	FalseSchema = &Schema{boolean: &[]bool{false}[0]}
)

// ----------------------------------------

type TestUser struct {
	ID      string                 `json:"id"`
	Name    string                 `json:"name"` // The name of a friend
	Friends []int                  `json:"friends,omitempty"`
	Tags    map[string]interface{} `json:"tags,omitempty" jsonschema_extras:"a=b,foo=bar,foo=bar1"` // TODO: dynamic types (a,foo)

	BirthDate   time.Time `json:"birth_date,omitempty" jsonschema:"oneof_required=date"` // TODO: oneof_required
	YearOfBirth string    `json:"year_of_birth,omitempty" jsonschema:"oneof_required=year"`

	Metadata interface{} `json:"metadata,omitempty" jsonschema:"oneof_type=string;array"` // oneOf

	FavColor string `json:"fav_color,omitempty"`
}

func main() {
	rec := NewRecorder()

	{
		st := rec.TypeOf(&TestUser{})

		st.FieldByName("Name").Title("the name").Example("joe").Example("lucy").Default("alex")
		st.FieldByName("Friends").Description("The list of IDs, omitted when empty")
		st.FieldByName("FavColor").Enum("red", "green", "blue")
	}

	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(rec); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
