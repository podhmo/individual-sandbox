package main

import (
	"fmt"
	"reflect"
	"strings"
	"testing"
	"time"
	rcmp "github.com/podhmo/rcmp" // まだない
)

type Person struct {
	Name string
	Age  int

	CreatedAt time.Time

	memo string // 無視される

	Mother *Person
	Father *Person

	Children []*Person
}

func TestIt(t *testing.T) {
	ob := Person{
		Name:      "foo",
		Age:       20,
		CreatedAt: time.Now(),
		Mother: &Person{
			Name:      "bar",
			Age:       40,
			CreatedAt: time.Now(),
			memo:      "xxx",
		},
		Children: []*Person{
			{
				Name:      "boo",
				CreatedAt: time.Now(),
				Age:       1,
			},
		},
	}

	var z time.Time
	another := Person{
		Name:      "foo",
		Age:       20,
		CreatedAt: z,
		Mother: &Person{
			Name:      "bar",
			Age:       40,
			memo:      "xxx",
			CreatedAt: z,
		},
		Children: []*Person{
			{
				Name:      "boo",
				Age:       1,
				CreatedAt: z,
			},
		},
	}

	// rcmp.Equal() returns bool, rcmp.NoDiff() returns error
	err := rcmp.NoDiff(ob, another,
		rcmp.WithIntercept(map[reflect.Type]rcmp.CompareFunc{
			reflect.TypeOf(z): func(path string, left interface{}, right interface{}) error {
				if path == "Mother.CreatedAt" {
					// 特定のpathのときだけの比較がほしい
				}
				if !reflect.DeepEqual(left, right) {
					return fmt.Errorf("%s: got %v, but want %v", path, left, right)
				}
			},
			reflect.TypeOf([]*Person{}): func(path string, left interface{}, right interface{}) error {
				if strings.HasSuffix(path, ".Children") {
					// leftとrightをsortしてから比較 (副作用ありで良い)
					sort(left)
				}
				// 既存の比較フローに戻す
				return rcmp.DelegateToDefault
			},
		}),
	)
	if err != nil {
		t.Errorf("!! %+v", err)
	}
}
