package main

import (
	"fmt"
	"log"
	"m/reflectwalk"
	"strings"
	"time"

	"github.com/k0kubun/pp"
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
	refs := map[string]interface{}{}

	err := reflectwalk.ReflectWalk(dummy, func(path []string, v interface{}) {
		switch v.(type) {
		case time.Time:
			refs[fmt.Sprintf("#/%s", strings.Join(path, "/"))] = v
		}
	})
	if err != nil {
		return err
	}
	pp.Println(dummy)
	pp.Println(refs)
	return nil
}
