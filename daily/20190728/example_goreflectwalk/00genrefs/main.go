package main

import (
	"fmt"
	"log"
	"m/reflectwalk"
	"strings"
	"time"

	"github.com/k0kubun/pp"
	rfc3339 "github.com/podhmo/go-rfc3339"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("+%v", err)
	}
}

// Dummy :
type Dummy struct {
	Name string    `json:"name"`
	Now  time.Time `json:"now"`
}

func (d *Dummy) String() string { return "dummy" + d.Name }

func run() error {
	type Wrap struct {
		Val     Dummy                `json:"val"`
		Ptr     *Dummy               `json:"ptr"`
		Skip    *Dummy               `json:"-"`
		Null    *Dummy               `json:"null"`
		Iface   interface{}          `json:"iface"`
		Slices  []Dummy              `json:"slices"`
		Slices2 []*Dummy             `json:"slices2"`
		Slices3 []interface{}        `json:"slices2"`
		Slices4 []Dummy              `json:"slices4"`
		Slices5 []Dummy              `json:"slices5"`
		Array   [1]Dummy             `json:"array"`
		Map     map[string]*Dummy    `json:"map"`
		Map2    map[int]interface{}  `json:"map2"`
		Map3    map[time.Time]*Dummy `json:"map3"`
	}

	dummy := &Dummy{Name: "foo", Now: time.Now()}
	now := rfc3339.MustParse("2000-01-01T00:00:00Z")
	wrap := &Wrap{
		Val:     *dummy,
		Ptr:     dummy,
		Skip:    dummy,
		Iface:   dummy,
		Slices:  []Dummy{*dummy},
		Slices2: []*Dummy{dummy},
		Slices3: []interface{}{dummy},
		Slices4: []Dummy{},
		Slices5: nil,
		Array:   [1]Dummy{*dummy},
		Map:     map[string]*Dummy{"foo": dummy},
		Map2:    map[int]interface{}{10: dummy},
		Map3:    map[time.Time]*Dummy{now: dummy},
	}
	refs := map[string]interface{}{}

	err := reflectwalk.ReflectWalk(wrap, func(path []string, v interface{}) {
		pp.Println("!!", v)

		switch v.(type) {
		case time.Time:
			refs[fmt.Sprintf("#/%s", strings.Join(path, "/"))] = v
		}
	})
	if err != nil {
		return err
	}
	pp.Println(wrap)
	pp.Println(refs)
	return nil
}
