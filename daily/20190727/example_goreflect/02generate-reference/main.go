package main

import (
	"fmt"
	"log"
	"m/walk"
	"strings"
	"time"

	"github.com/k0kubun/pp"
	"github.com/podhmo/go-webtest/jsonequal"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("+%v", err)
	}
}
func run() error {
	type Dummy struct {
		Name string    `json:"name"`
		Now  time.Time `json:"now"`
	}

	dummy := &Dummy{Name: "foo", Now: time.Now()}
	data := jsonequal.MustNormalize(dummy)
	refs := map[string]interface{}{}

	walk.Walk(data, func(path []string, v interface{}) {
		switch v := v.(type) {
		case *time.Time:
			if v != nil {
				refs[fmt.Sprintf("#/%s", strings.Join(path, "/"))] = *v
			}
		case time.Time:
			refs[fmt.Sprintf("#/%s", strings.Join(path, "/"))] = v
		}
	})

	pp.Println(dummy)
	pp.Println(refs)
	return nil
}
