package main

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"reflect"
	"strconv"
	"strings"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type S struct {
	Name   string                    `validate:"pattern=[A-Z].*,notzero"`
	Links  []string                  `validate:"min-length=1,@pattern=\\d+$,unique"`
	Items  map[string]map[string]int `validate:"@@positive,@max-length=99"`
	Links2 *[]*string                `validate:"min-length=1,@pattern=\\d+$,unique"`
	xs     []string                  `validate:"hmm"`
}

type Kludge struct {
	rt   reflect.Type
	Code []Cell
}

func (k *Kludge) Describe() string {
	buf := bytes.NewBuffer(nil)
	for _, code := range k.Code {
		switch code.Fn {
		case FnField:
			io.WriteString(buf, "@")
			io.WriteString(buf, code.Args[0])
			io.WriteString(buf, " ")
		case FnDeField:
			io.WriteString(buf, "\n")
		case FnMap:
			io.WriteString(buf, "{ ")
		case FnDeMap:
			io.WriteString(buf, " }")
		case FnSlice:
			io.WriteString(buf, "[ ")
		case FnDeSlice:
			io.WriteString(buf, " ]")
		case FnCall:
			io.WriteString(buf, code.Args[0])
			io.WriteString(buf, "(")
			io.WriteString(buf, strconv.Quote(code.Args[1]))
			io.WriteString(buf, "), ")
		default:
			io.WriteString(buf, string(code.Fn))
			io.WriteString(buf, ", ")
		}
	}
	return buf.String()
}

type Cell struct {
	Fn   Fn
	Args [2]string
}
type Fn string

const (
	FnField   Fn = "field"
	FnDeField    = "defield"
	FnSlice      = "slice"
	FnDeSlice    = "deslice"
	FnMap        = "map"
	FnDeMap      = "demap"
	FnPtr        = "ptr"
	FnDePtr      = "deptr"
	FnCall       = "call"
	FnEnd        = "end"
)

func Scan(ob interface{}, tag string) Kludge {
	rt := reflect.TypeOf(ob)
	if rt.Kind() == reflect.Ptr {
		rt = rt.Elem()
	}

	var code []Cell
	const maxN = 8
	for i := 0; i < rt.NumField(); i++ {
		rf := rt.Field(i)
		if !('A' <= rf.Name[0] && rf.Name[0] <= 'Z') {
			continue // unexported field
		}
		s, ok := rf.Tag.Lookup(tag)
		if !ok {
			continue
		}
		code = append(code, Cell{Fn: FnField, Args: [2]string{rf.Name, ""}})

		var buf [maxN][]Cell
		parts := strings.Split(strings.TrimSpace(s), ",")
		for _, x := range parts {
			kv := strings.SplitN(x, "=", 2)
			rawK := strings.TrimSpace(kv[0])
			k := strings.TrimLeft(rawK, "@")
			lv := len(rawK) - len(k)
			if lv > maxN {
				panic("too many depth")
			}
			if len(kv) == 1 {
				buf[lv] = append(buf[lv], Cell{Fn: FnCall, Args: [2]string{k, ""}})
				continue
			}
			buf[lv] = append(buf[lv], Cell{Fn: FnCall, Args: [2]string{k, strings.TrimSpace(kv[1])}})
		}

		typ := rf.Type
		rbuf := make([]Cell, 0, maxN)
	loop:
		for i := 0; i < maxN; i++ {
			if typ.Kind() == reflect.Ptr {
				code = append(code, Cell{Fn: FnPtr})
				typ = typ.Elem()
			}
			switch typ.Kind() {
			case reflect.Slice:
				code = append(code, Cell{Fn: FnSlice})
				typ = typ.Elem()
				rbuf = append(rbuf, Cell{Fn: FnDeSlice})
			case reflect.Map:
				code = append(code, Cell{Fn: FnMap})
				typ = typ.Elem()
				rbuf = append(rbuf, Cell{Fn: FnDeMap})
			default:
				if len(buf[i]) > 0 {
					code = append(code, buf[i]...)
				}
				break loop
			}
			if len(buf[i]) > 0 {
				code = append(code, buf[i]...)
			}
		}

		for i := len(rbuf); i > 0; i-- {
			code = append(code, rbuf[i-1])
		}
		code = append(code, Cell{Fn: FnDeField})
	}
	return Kludge{rt: rt, Code: code}
}

func run() error {
	k := Scan(S{}, "validate")
	fmt.Println(k.Describe())
	return nil
}
