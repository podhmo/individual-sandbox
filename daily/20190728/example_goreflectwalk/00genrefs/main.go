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
	type Wrap struct {
		Val     Dummy               `json:"val"`
		Ptr     *Dummy              `json:"ptr"`
		Skip    *Dummy              `json:"-"`
		Null    *Dummy              `json:"null"`
		Iface   interface{}         `json:"iface"`
		Slices  []Dummy             `json:"slices"`
		Slices2 []*Dummy            `json:"slices2"`
		Slices3 []interface{}       `json:"slices2"`
		Map     map[string]*Dummy   `json:"map"`
		Map2    map[int]interface{} `json:"map2"`
	}

	dummy := &Dummy{Name: "foo", Now: time.Now()}
	wrap := &Wrap{
		Val:     *dummy,
		Ptr:     dummy,
		Skip:    dummy,
		Iface:   dummy,
		Slices:  []Dummy{*dummy},
		Slices2: []*Dummy{dummy},
		Slices3: []interface{}{dummy},
		Map:     map[string]*Dummy{"foo": dummy},
		Map2:    map[int]interface{}{10: dummy},
	}
	refs := map[string]interface{}{}

	err := reflectwalk.ReflectWalk(wrap, func(path []string, v interface{}) {
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
