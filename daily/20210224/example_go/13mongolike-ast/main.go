package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"reflect"
	"strconv"

	"github.com/davecgh/go-spew/spew"
)

type Node interface{ node() }

type Uop = string

const (
	UopEQ  Uop = "$eq"
	UopGT      = "$gt"
	UopGTE     = "$gte"
	UopIN      = "$in"
	UopLT      = "$lt"
	UopLTE     = "$lte"
	UopNE      = "$ne"
	UopNIN     = "$nin"
)

type U struct {
	Op    Uop
	Value interface{}
}

func (u U) node() {}

type Mop = string

const (
	MopAND   Mop = "$and"
	MopNOT       = "$not"
	MopNOR       = "$nor"
	MopOR        = "$or"
	MopEMPTY     = "" // treated as $and
)

// $and,$orとobjectを一緒に考えないほうが良いのでは？
type M struct {
	Op     Mop
	Values []Node
}

func (m M) node() {}

type Map struct {
	Values []Node
}

func (m Map) node() {}

type Named struct {
	Name  string
	Value Node
}

func (n Named) node() {}

type Bool bool

func (b Bool) node() {}

func Parse(vs map[string]interface{}) (Node, error) {
	return parse(vs, nil)
}

func parse(val interface{}, path []string) (Node, error) {
	switch val := val.(type) {
	case []interface{}:
		return parseArray(val, path)
	case map[string]interface{}:
		return parseMap(val, path)
	case bool:
		return Bool(val), nil
	default: // int, string
		return Bool(false), fmt.Errorf("invalid %v, path=%q", val, path)
	}
}
func parseArray(val []interface{}, path []string) (Node, error) {
	m := M{Op: path[len(path)-1]}
	for i, v := range val {
		child, err := parse(v, append(path, strconv.Itoa(i)))
		if err != nil {
			return Bool(false), fmt.Errorf("invalid %v, path=%q", val, append(path, strconv.Itoa(i)))
		}
		m.Values = append(m.Values, child)
	}
	return m, nil
}
func parseMap(val map[string]interface{}, path []string) (Node, error) {
	m := Map{}
	hasConditional := false
	for k, v := range val {
		switch k {
		case UopEQ, UopGT, UopGTE, UopIN, UopLT, UopLTE, UopNE, UopNIN: // U
			// assert v is map
			if len(val) != 1 {
				return Bool(false), fmt.Errorf("invalid uop %v, path=%q", val, append(path, k))
			}
			if x, ok := v.(float64); ok {
				if float64(int64(x)) == x {
					v = int64(x) // int?, int64?
				}
			}
			return U{Op: k, Value: v}, nil
		case MopAND, MopNOT, MopNOR, MopOR: // M
			hasConditional = true
			child, err := parse(v, append(path, k))
			if err != nil {
				return Bool(false), fmt.Errorf("invalid mop %v, path=%q", val, append(path, k))
			}
			m.Values = append(m.Values, child)
		default: // Named
			child, err := parse(v, append(path, k))
			if err != nil {
				return Bool(false), fmt.Errorf("invalid named %v, path=%q", val, append(path, k))
			}
			m.Values = append(m.Values, Named{Name: k, Value: child})
		}
	}

	// simplify
	if hasConditional && len(m.Values) == 1 {
		return m.Values[0], nil
	}
	return m, nil
}

func main() {
	i := 0

	{
		fmt.Println("")
		fmt.Println(`db.inventory.find( { qty: { $gt: 20 } } )`)
		m := Map{Values: []Node{
			Named{Name: "qty", Value: U{Op: UopGT, Value: int64(20)}},
		}}

		fmt.Println(m)

		vs := map[string]interface{}{}
		json.Unmarshal([]byte(`{"qty": {"$gt": 20}}`), &vs)
		m2, err := Parse(vs)
		fmt.Printf("ok?=%v, %#+v, err=%+v\n", reflect.DeepEqual(m, m2), m2, err)
		i++
		ioutil.WriteFile(fmt.Sprintf("/tmp/before.%d", i), []byte(spew.Sdump(m)), 0744)
		ioutil.WriteFile(fmt.Sprintf("/tmp/after.%d", i), []byte(spew.Sdump(m2)), 0744)

	}
	{
		fmt.Println("")
		fmt.Println(`db.inventory.find( { xxx.qty: { $gt: 20 } } )`)
		m := Map{Values: []Node{
			Named{Name: "xxx.qty", Value: U{Op: UopGT, Value: int64(20)}},
		}}
		fmt.Println(m)

		vs := map[string]interface{}{}
		json.Unmarshal([]byte(`{"xxx.qty": {"$gt": 20}}`), &vs)
		m2, err := Parse(vs)
		fmt.Printf("ok?=%v, %#+v, err=%+v\n", reflect.DeepEqual(m, m2), m2, err)
		i++
		ioutil.WriteFile(fmt.Sprintf("/tmp/before.%d", i), []byte(spew.Sdump(m)), 0744)
		ioutil.WriteFile(fmt.Sprintf("/tmp/after.%d", i), []byte(spew.Sdump(m2)), 0744)
	}

	// 無理
	{
		fmt.Println("")
		fmt.Println(`db.inventory.find( { qty: { $gt: 20, $lt: 50  } } )`)
		fmt.Println(`
db.inventory.find( { $and: [
  qty: { $gt: 20 },
  qty: { $lt: 50 },
] } )`)
		m := M{
			Op: MopAND,
			Values: []Node{
				Named{
					Name:  "qty",
					Value: U{Op: UopGT, Value: int64(20)},
				},
				Named{
					Name:  "qty",
					Value: U{Op: UopLT, Value: int64(50)},
				},
			},
		}
		fmt.Println(m)

		vs := map[string]interface{}{}
		json.Unmarshal([]byte(`
{ "$and": [
  {"qty": { "$gt": 20 }},
  {"qty": { "$lt": 50 }}
] }
`), &vs)
		m2, err := Parse(vs)
		fmt.Printf("ok?=%v, %#+v, err=%+v\n", reflect.DeepEqual(m, m2), m2, err)

		i++
		ioutil.WriteFile(fmt.Sprintf("/tmp/before.%d", i), []byte(spew.Sdump(m)), 0744)
		ioutil.WriteFile(fmt.Sprintf("/tmp/after.%d", i), []byte(spew.Sdump(m2)), 0744)
	}
	{
		fmt.Println("")
		fmt.Println(`db.inventory.find( { xxx: {qty: { $gt: 20 } }, "yyy": { $eq: 10  } } )`)
		m := Map{
			Values: []Node{
				Named{Name: "xxx", Value: Map{
					Values: []Node{
						Named{Name: "qty", Value: U{Op: UopGT, Value: int64(20)}},
					},
				}},
				Named{Name: "yyy", Value: U{Op: UopEQ, Value: int64(10)}},
			},
		}
		fmt.Println(m)

		vs := map[string]interface{}{}
		json.Unmarshal([]byte(`{ "xxx": {"qty": { "$gt": 20 } }, "yyy": { "$eq": 10  } }`), &vs)
		m2, err := Parse(vs)
		fmt.Printf("ok?=%v, %#+v, err=%+v\n", reflect.DeepEqual(m, m2), m2, err)

		i++
		ioutil.WriteFile(fmt.Sprintf("/tmp/before.%d", i), []byte(spew.Sdump(m)), 0744)
		ioutil.WriteFile(fmt.Sprintf("/tmp/after.%d", i), []byte(spew.Sdump(m2)), 0744)
	}

	{
		fmt.Println("")
		fmt.Print(`
db.inventory.find( {
    $and: [
        { $or: [ { qty: { $lt : 10 } }, { qty : { $gt: 50 } } ] },
        { $or: [ { sale: true }, { price : { $lt : 5 } } ] }
    ]
} )
`)
		m := M{
			Op: MopAND,
			Values: []Node{
				M{
					Op: MopOR,
					Values: []Node{
						Map{Values: []Node{Named{Name: "qty", Value: U{Op: UopLT, Value: int64(10)}}}},
						Map{Values: []Node{Named{Name: "qty", Value: U{Op: UopGT, Value: int64(50)}}}},
					},
				},
				M{
					Op: MopOR,
					Values: []Node{
						Map{Values: []Node{Named{Name: "sale", Value: Bool(true)}}},
						Map{Values: []Node{Named{Name: "price", Value: U{Op: UopLT, Value: int64(5)}}}},
					},
				},
			},
		}

		fmt.Println(m)

		vs := map[string]interface{}{}
		json.Unmarshal([]byte(`
{
	    "$and": [
	        { "$or": [ { "qty": { "$lt": 10 } }, { "qty": { "$gt": 50 } } ] },
	        { "$or": [ { "sale": true }, { "price": { "$lt": 5 } } ] }
	    ]
	}
`), &vs)
		m2, err := Parse(vs)
		fmt.Printf("ok?=%v, %#+v, err=%+v\n", reflect.DeepEqual(m, m2), m2, err)
		// normalizeを取り入れるべき？$or,$andにすぐにNamedが来ることを許すべきなんだろうか？

		i++
		ioutil.WriteFile(fmt.Sprintf("/tmp/before.%d", i), []byte(spew.Sdump(m)), 0744)
		ioutil.WriteFile(fmt.Sprintf("/tmp/after.%d", i), []byte(spew.Sdump(m2)), 0744)
	}
}
