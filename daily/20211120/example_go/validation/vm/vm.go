package vm

import (
	"fmt"
	"os"
	"reflect"
)

// todo: rename node
type Command Op

type Op interface {
	Op() string
}

type Type struct {
	Name    string
	PkgPath string
	RT      reflect.Type
	Lv      int // if pointer lv=1
}
type Struct struct {
	Type
	Fields []*Field
}
type Field struct {
	Name    string
	Type    Type
	Tag     reflect.StructTag
	Options []*Option
}
type Option struct {
	Name string
	Args []interface{}
}

func (op *Struct) Op() string { return "Struct" }
func (op *Field) Op() string  { return "Field" }
func (op *Option) Op() string { return "Option" }

type CommandEmitter struct {
	Command []Command
	Debug   bool

	shared map[reflect.Type]*Struct
	field  *Field
}

func typeOf(rt reflect.Type) Type {
	typ := Type{}
	if rt.Kind() == reflect.Ptr { // todo: support **<T>
		rt = rt.Elem()
		typ.Lv++
	}

	typ.Name = rt.Name()
	typ.PkgPath = rt.PkgPath()
	typ.RT = rt
	return typ
}

func (e *CommandEmitter) EmitStruct(rt reflect.Type) *Struct {
	if e.shared == nil {
		e.shared = map[reflect.Type]*Struct{}
	}
	op := &Struct{Type: typeOf(rt)}
	op.Fields = []*Field{}

	e.Command = append(e.Command, op)
	e.shared[op.RT] = op

	if e.Debug {
		fmt.Fprintf(os.Stderr, "\tStruct\tname:%s\tlv:%d\tpath:%q\n", op.Name, op.Lv, op.PkgPath)
	}
	return op
}

func (e *CommandEmitter) EmitField(rt reflect.Type, f reflect.StructField) *Field {
	op := &Field{}
	op.Name = f.Name
	op.Type = typeOf(f.Type)
	op.Tag = f.Tag

	s := e.shared[rt]
	s.Fields = append(s.Fields, op)

	e.field = op

	if e.Debug {
		fmt.Fprintf(os.Stderr, "\tField\tname:%s\ttype:%s\tlv:%d\tpath:%q\n", op.Name, op.Type.Name, op.Type.Lv, op.Type.PkgPath)
	}
	return op
}

func (e *CommandEmitter) EmitOption(name string, args ...interface{}) *Option {
	f := e.field // xxx:

	op := &Option{}
	op.Name = name
	op.Args = args
	if op.Args == nil {
		op.Args = []interface{}{}
	}

	f.Options = append(f.Options, op)

	if e.Debug {
		fmt.Fprintf(os.Stderr, "\tOption\tname:%s\n", op.Name)
	}
	return op
}
