package main

import (
	"fmt"
	"log"
	"reflect"
	"strings"
)

func main() {
	log.SetFlags(0)

	type CreatePersonInput struct {
		Name string
		Age  int
	}

	rt := reflect.TypeOf(CreatePersonInput{})
	code := Scan(rt) // 一度生成したらcache

	input := &CreatePersonInput{Name: "foo", Age: 20}
	fmt.Printf("before: %#+v\n", input)
	Apply(input, code)
	fmt.Printf("after : %#+v\n", input)
}

// 型を見て動作を生成 (e.g. 例えば入力に対するvalidationなど)
func Scan(rt reflect.Type) []Op {
	return Code(
		Elem(), OpPush,
		FieldByName(rt, "Name"), OpPush, SetString("*bar*"), OpPop,
		FieldByName(rt, "Age"), OpPush, SetInt(100), OpPop,
		OpPop)
}

type opType int

const (
	opInvalid opType = iota
	opElem
	opField
	opSetString
	opSetInt

	opPush
	opPop
)

var opMessages = []string{
	"e:invalid",
	"c:elem", "c:field", "c:set-string", "c:set-int",
	"a:push", "a:pop",
}

func (t opType) String() string {
	return opMessages[int(t)]
}

type Op struct {
	Type opType
	I    int
	S    string
}

var (
	OpInvalid = Op{Type: opInvalid}
	OpElem    = Op{Type: opElem}
	OpPush    = Op{Type: opPush}
	OpPop     = Op{Type: opPop}
)

func FieldByName(rt reflect.Type, name string) Op {
	for i := 0; i < rt.NumField(); i++ {
		rf := rt.Field(i)
		if rf.Name == name {
			return Op{Type: opField, I: i}
		}
	}
	return OpInvalid // TODO: stack
}

func Elem() Op {
	return OpElem // TODO: stack
}

func SetString(value string) Op {
	return Op{Type: opSetString, S: value}
}
func SetInt(value int) Op {
	return Op{Type: opSetInt, I: value}
}

func Code(ops ...Op) []Op {
	return ops
}

func Apply[T any](ob *T, code []Op) {
	vm := &VM{}
	vm.stack = append(vm.stack, reflect.ValueOf(ob))
	i := 0

	val := vm.stack[i]
	for _, op := range code {
		log.Printf("%-5s\top=%15v\tstack=%s", strings.Repeat("#", len(vm.stack)), op, vm.stack)
		switch op.Type {
		case opInvalid:
			panic("hmm")
		case opPush:
			// log.Printf("\tpush: %v, in %v", val, vm.stack)
			vm.stack = append(vm.stack, val)
			i++
		case opPop:
			// pv := vm.stack[i]
			vm.stack = vm.stack[:i]
			i--
			// log.Printf("\t pop: %v, in %v", pv, vm.stack)
			val = vm.stack[i]
		case opElem:
			val = val.Elem()
		case opField:
			val = val.Field(op.I)
		case opSetString:
			val.SetString(op.S)
		case opSetInt:
			val.SetInt(int64(op.I))
		}
	}
}

type VM struct {
	stack []reflect.Value
}
