package main

import (
	"encoding/json"
	"fmt"
	"log"
	"math"
	"os"
	"reflect"
	"strings"
	"time"

	"github.com/podhmo/validator/tagscan"
	ordered "gitlab.com/c0b/go-ordered-json"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type Person struct {
	Name     string           `json:"name" validate:"@foo"`
	Age      int              `json:"age"`
	Children []*Person        `json:"children"`
	Items    []map[string]int `json:"items"`

	Messages []Message
}

type Message struct {
	Title string
}

func run() error {
	cfg := tagscan.NewConfigDefault()
	cfg.FullScan = true
	cfg.FullScanDefault = "ELT"

	s := cfg.Scanner()
	kludges, err := s.ScanAll(Person{})
	if err != nil {
		return err
	}

	w := &Walker{
		Schemas: ordered.NewOrderedMap(),
		Resolver: &Resolver{
			RefFormat:     "#/components/schemas/%s",
			StrictDefault: true,
		},
		Lookup: s.Lookup,
		seen:   map[*tagscan.Kludge]bool{},
	}
	w.queue = append(w.queue, kludges...)
	for {
		if len(w.queue) == 0 {
			break
		}

		queue := w.queue[:]
		w.queue = nil
		for _, kludge := range queue {
			// fmt.Println(kludge.Describe())
			if err := w.Walk(kludge); err != nil {
				return err
			}
		}
	}

	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	enc.Encode(w.Schemas)
	return nil
}

type Walker struct {
	Schemas  *ordered.OrderedMap
	Resolver *Resolver
	Lookup   func(reflect.Type) *tagscan.Kludge

	queue []*tagscan.Kludge
	seen  map[*tagscan.Kludge]bool
}
type Resolver struct {
	RefFormat     string
	StrictDefault bool
}

func (r *Resolver) Title(rt reflect.Type) string {
	// TODO: prevent conflict
	name := rt.String()
	return name[strings.LastIndex(name, ".")+1:]
}

func (r *Resolver) Ref(t reflect.Type) string {
	return fmt.Sprintf(r.RefFormat, r.Title(t))
}

func (r *Resolver) BindType(prop *ordered.OrderedMap, t reflect.Type) error {
	switch t.Kind() {
	case reflect.Func, reflect.Chan:
		prop.Set("x-go-type", t.String())

	case reflect.Bool:
		prop.Set("type", "boolean")

	case reflect.Int:
		prop.Set("type", "integer")
	case reflect.Int8:
		prop.Set("type", "integer")
		prop.Set("minimum", int64(minInt8))
		prop.Set("maximum", int64(maxInt8))
	case reflect.Int16:
		prop.Set("type", "integer")
		prop.Set("minimum", int64(minInt16))
		prop.Set("maximum", int64(maxInt16))
	case reflect.Int32:
		prop.Set("type", "integer")
		prop.Set("format", "int32")
	case reflect.Int64:
		prop.Set("type", "integer")
		prop.Set("format", "int64")
	case reflect.Uint8:
		prop.Set("type", "integer")
		prop.Set("minimum", 0)
		prop.Set("maximum", int64(maxUint8))
	case reflect.Uint16:
		prop.Set("type", "integer")
		prop.Set("minimum", 0)
		prop.Set("maximum", int64(maxUint16))
	case reflect.Uint32:
		prop.Set("type", "integer")
		prop.Set("minimum", 0)
		prop.Set("maximum", int64(maxUint32))
	case reflect.Uint64:
		prop.Set("type", "integer")
		prop.Set("minimum", 0)
		prop.Set("maximum", int64(maxUint64))
	case reflect.Float32:
		prop.Set("type", "number")
		prop.Set("format", "float")
	case reflect.Float64:
		prop.Set("type", "number")
		prop.Set("format", "double")

	case reflect.String:
		prop.Set("type", "string")

	case reflect.Slice:
		prop.Set("type", "-")
	case reflect.Map:
		prop.Set("type", "-")
	case reflect.Struct:
		if t == timeType {
			prop.Set("type", "string")
			prop.Set("format", "date-time")
		} else {
			prop.Set("type", "object")
			prop.Set("additionalProperties", false)
		}
	default:
		return fmt.Errorf("unexpected kind %s", t)
	}
	return nil
}

var (
	timeType       = reflect.TypeOf(time.Time{})
	rawMessageType = reflect.TypeOf(json.RawMessage{})

	zeroInt   = float64(0)
	maxInt8   = float64(math.MaxInt8)
	minInt8   = float64(math.MinInt8)
	maxInt16  = float64(math.MaxInt16)
	minInt16  = float64(math.MinInt16)
	maxUint8  = float64(math.MaxUint8)
	maxUint16 = float64(math.MaxUint16)
	maxUint32 = float64(math.MaxUint32)
	maxUint64 = float64(math.MaxUint64)
)

func (w *Walker) Walk(kludge *tagscan.Kludge) error {
	if _, ok := w.seen[kludge]; ok {
		return nil
	}
	w.seen[kludge] = true

	root := kludge.Type
	t := root
	pc := 0
	code := kludge.Code

	s := ordered.NewOrderedMap()
	if err := w.Resolver.BindType(s, t); err != nil {
		return err
	}

	props := ordered.NewOrderedMap()
	s.Set("properties", props)
	s.Set("x-go-type", t.String())
	w.Schemas.Set(w.Resolver.Title(kludge.Type), s)

	var name string
	var prop, inner *ordered.OrderedMap
loop:
	for {
		cell := code[pc]
		switch cell.Op {
		case tagscan.OpEnd:
			break loop
		case tagscan.OpPtr:
			t = t.Elem()
		case tagscan.OpField:
			rf, ok := root.FieldByName(cell.Args[0])
			if !ok {
				return fmt.Errorf("field %s is not found", name)
			}
			t = rf.Type
			name = cell.Args[1] // json tag
			if name == "" {
				name = cell.Args[0]
			}
			prop = ordered.NewOrderedMap()
			inner = prop
		case tagscan.OpDeField:
			props.Set(name, prop)
		case tagscan.OpSlice:
			wrap := inner
			inner = ordered.NewOrderedMap()
			wrap.Set("type", "array")
			wrap.Set("items", inner)
			t = t.Elem()
		case tagscan.OpDeSlice:
		case tagscan.OpMap:
			wrap := inner
			inner = ordered.NewOrderedMap()
			wrap.Set("type", "object")
			wrap.Set("additionalProperties", inner)
			t = t.Elem()
		case tagscan.OpDeMap:
		case tagscan.OpCall:
			if cell.Args[0] == "ELT" {
				switch t.Kind() {
				case reflect.Struct:
					w.queue = append(w.queue, w.Lookup(t))
					inner.Set("$ref", w.Resolver.Ref(t))
				default:
					if err := w.Resolver.BindType(inner, t); err != nil {
						return fmt.Errorf("field %s, guess type is failed (type=%v): %w", name, t, err)
					}
				}
			}
		default:
			return fmt.Errorf("unexpected opcode %+v", cell)
		}
		pc++
	}
	// fmt.Printf("%[1]T: %+[1]v\n", kludge.Value)
	return nil
}
