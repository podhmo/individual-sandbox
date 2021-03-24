package main

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"reflect"
	"regexp"
	"strconv"
	"strings"
)

var rx = regexp.MustCompile(`(?:^|\s*)(@+[a-zA-Z_\-]+)\s*(?:=|,|$)`)

func Lex(s string) [][2]string {
	// BUG: not support like this. `@pattern=\\d+@xxx=+=$,@unique`, (work-around is `@pattern=\\d+[@]xxx=+=$,@unique` )

	// <predicate> :: { <attribute> '=' <args> }+
	// <attribute> :: { '@' }+ { <token> }+
	// <args> :: { <token> ','}* <token>

	indexList := rx.FindAllStringSubmatchIndex(s, -1)
	if len(indexList) == 0 {
		return nil
	}
	r := make([][2]string, 0, len(indexList))
	n := len(indexList)
	for i := 1; i < n; i++ {
		index := indexList[i-1]
		r = append(r, [2]string{
			s[index[2]:index[3]],
			strings.TrimSpace(strings.TrimSuffix(s[index[1]:indexList[i][0]], ",")),
		})
	}
	index := indexList[n-1]
	r = append(r, [2]string{
		s[index[2]:index[3]],
		strings.TrimSpace(s[index[1]:]),
	})
	return r
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
		if !strings.Contains(s, "@") {
			log.Printf("WARNING, %q, forget to add @? in %s.%s", rf.Tag, rt, rf.Name)
		}

		code = append(code, Cell{Fn: FnField, Args: [2]string{rf.Name, ""}})
		var buf [maxN][]Cell
		parts := Lex(s)
		for _, pair := range parts {
			rawK := pair[0]
			k := strings.TrimLeft(rawK, "@")
			lv := len(rawK) - len(k) - 1
			if lv > maxN {
				log.Printf("WARNING, the %q's nested level is too deep in %s.%s", rawK, rt, rf.Name)
			}
			buf[lv] = append(buf[lv], Cell{Fn: FnCall, Args: [2]string{k, pair[1]}})
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

type S struct {
	Name   string                    `validate:"@pattern=[A-Z].*,@notzero"`
	Links  []string                  `validate:"@min-length=1,@@pattern=\\d+$,@unique"`
	Items  map[string]map[string]int `validate:"@@@positive,@@max-length=99"`
	Links2 *[]*string                `validate:"@min-length=1,@@pattern=(\\d+,\\d+)$,@unique"`
	xs     []string                  `validate:"@hmm"` // unexported field is ignored
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	k := Scan(S{}, "validate")
	fmt.Println(k.Describe())
	return nil
}
