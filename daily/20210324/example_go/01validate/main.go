package main

import (
	"fmt"
	"log"
	"reflect"
	"regexp"
	"sync"

	"github.com/podhmo/validator/tag"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func Validate(s *tag.Scanner, ob interface{}) error {
	if ob == nil {
		return fmt.Errorf("nil") // error?
	}

	kludge, err := s.Scan(ob)
	if err != nil {
		return err
	}

	root := reflect.ValueOf(ob)
	for root.Kind() == reflect.Ptr {
		root = root.Elem()
	}

	fn := Regexp()

	// fmt.Fprintln(os.Stderr, kludge.Describe())

	x := root
	for _, cell := range kludge.Code {
		switch cell.Op {
		case tag.OpField:
			x = root.FieldByName(cell.Args[0])
		case tag.OpDeField:
		// case tag.OpMap:
		// case tag.OpDeMap:
		case tag.OpSlice:
			for i, n := 0, x.Len(); i < n; i++ {
				fmt.Println(i, n)
			}
		// case tag.OpDeSlice:
		case tag.OpCall:
			pattern := cell.Args[1]
			err := fn(pattern, x)
			if err != nil {
				return err
			}
		default:
			return fmt.Errorf("unexpected opcode %+v", cell)
		}
	}
	_ = x
	return nil
}

type Config struct {
	Color     string   `validate:"@regexp=^(#[0-9a-fA-F]{3}|)$"`
	ColorList []string `validate:"@@regexp=^#[0-9a-fA-F]{3}$"`
}

// https://json-schema.org/understanding-json-schema/reference/regular_expressions.html
func Regexp() func(string, reflect.Value) error {
	m := map[string]*regexp.Regexp{}
	var mu sync.RWMutex

	return func(pattern string, val reflect.Value) error {
		mu.RLock()
		rx, ok := m[pattern]
		mu.RUnlock()
		if !ok {
			x, err := regexp.Compile(pattern)
			if err != nil {
				return err
			}
			rx = x
			mu.Lock()
			m[pattern] = rx
			mu.Unlock()
		}
		if !rx.MatchString(val.String()) {
			return fmt.Errorf("%q is not matched (pattern=%s)", val, rx)
		}
		return nil
	}
}

func run() error {
	s := tag.NewScannerDefault()
	s.Tag = "validate"

	configList := []Config{
		// {Color: "#999"},
		// {Color: "#999x"},
		{ColorList: []string{"#999x"}},
	}
	for _, c := range configList {
		log.Printf("input %#+v", c)
		err := Validate(s, c)
		if err != nil {
			log.Printf("!! %+v", err)
		} else {
			log.Printf("ok")
		}
	}
	return nil
}
