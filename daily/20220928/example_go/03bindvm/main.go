package main

import (
	"fmt"
	"log"
	"net/http"
	"reflect"
	"strconv"
	"strings"
)

func main() {
	log.SetFlags(0)
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type Sort string

func run() error {
	type Input struct {
		Size  int64
		Sort  string // "id","-id"
		Sort2 Sort   // "id","-id"
		Debug bool
	}
	parse := Scan[Input]()
	req, err := http.NewRequest("GET", "/items?sort=-id&size=100&sort2=-id", nil)
	if err != nil {
		return err
	}
	input, err := parse(req)
	if err != nil {
		return err
	}
	fmt.Printf("%+v\n", input)
	return nil
}

func Scan[T any]() func(*http.Request) (T, error) {
	var ob T
	rt := reflect.TypeOf(ob)
	code := Code( // 本当は生成
		OpElem, OpPush,
		BindIntQuery(rt, "size", "Size"),
		BindStringEnumsQuery(rt, "sort", "Sort", []string{"id", "-id"}),
		BindStringEnumsQuery(rt, "sort2", "Sort2", []string{"id", "-id"}),
		OpPop,
	)
	return func(req *http.Request) (T, error) {
		var input T
		if err := Apply(req, &input, code); err != nil {
			return input, fmt.Errorf("apply: %w", err)
		}
		return input, nil
	}
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

	opParseQuery
	opBindIntQuery
	opBindStringEnumsQuery
)

var opMessages = []string{
	"e:invalid",
	"c:elem", "c:field", "c:set-string", "c:set-int",
	"a:push", "a:pop",
	"x:parse-query", "x:bind-int-query", "x:bind-string-enums-query",
}

func (t opType) String() string {
	return opMessages[int(t)]
}

type Op struct {
	Type opType
	I    int
	S    string
	S2   string
	SS   []string
}

var (
	OpInvalid = Op{Type: opInvalid}
	OpElem    = Op{Type: opElem}

	OpPush = Op{Type: opPush}
	OpPop  = Op{Type: opPop}

	OpParseQuery = Op{Type: opParseQuery}
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

func SetString(value string) Op {
	return Op{Type: opSetString, S: value}
}
func SetInt(value int) Op {
	return Op{Type: opSetInt, I: value}
}
func BindIntQuery(rt reflect.Type, keyname, fieldname string) Op { // todo: default
	for i := 0; i < rt.NumField(); i++ {
		rf := rt.Field(i)
		if rf.Name == fieldname {
			return Op{Type: opBindIntQuery, S: keyname, S2: fieldname, I: i}
		}
	}
	return Op{Type: opInvalid, S: keyname, S2: fieldname}
}

func BindStringEnumsQuery(rt reflect.Type, keyname, fieldname string, enums []string) Op { // todo: default
	for i := 0; i < rt.NumField(); i++ {
		rf := rt.Field(i)
		if rf.Name == fieldname {
			return Op{Type: opBindStringEnumsQuery, S: keyname, S2: fieldname, SS: enums, I: i}
		}
	}
	return Op{Type: opInvalid, S: keyname, S2: fieldname, SS: enums}
}
func Code(ops ...Op) []Op {
	return ops
}

func Apply[T any](req *http.Request, ob *T, code []Op) error {
	vm := &VM{}
	vm.stack = append(vm.stack, reflect.ValueOf(ob))
	i := 0

	val := vm.stack[i]
	q := req.URL.Query()
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
		case opBindIntQuery:
			key := op.S
			i := op.I
			if v := q.Get(key); v != "" {
				n, err := strconv.ParseInt(v, 10, 64)
				if err != nil {
					return fmt.Errorf("parse int %s: %w (value=%v)", key, err, v)
				}
				val.Field(i).SetInt(n)
			}
		case opBindStringEnumsQuery:
			key := op.S
			i := op.I
			if v := q.Get(key); v != "" {
				found := false
				for _, x := range op.SS {
					if x == v {
						val.Field(i).SetString(v)
						found = true
						break
					}
				}
				if !found {
					return fmt.Errorf("parse string enums %s: (value=%v)", key, v)
				}
			}
		}
	}
	return nil
}

type VM struct {
	stack []reflect.Value
}
