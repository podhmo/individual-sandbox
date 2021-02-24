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

type Named struct {
	Name   string
	Values []Node
}

func (n Named) node() {}

type Bool bool

func (b Bool) node() {}

func Parse(vs map[string]interface{}) (M, error) {
	node, err := parse(vs, nil)
	if err != nil {
		return M{}, err
	}
	return node.(M), nil
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
	m := M{}
	hasConditional := false
	for k, v := range val {
		switch k {
		case UopEQ, UopGT, UopGTE, UopIN, UopLT, UopLTE, UopNE, UopNIN: // U
			if x, ok := v.(float64); ok {
				if float64(int64(x)) == x {
					v = int64(x) // int?, int64?
				}
			}
			m.Values = append(m.Values, U{Op: k, Value: v})
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
			if x, ok := child.(M); ok && x.Op == MopEMPTY {
				m.Values = append(m.Values, Named{Name: k, Values: x.Values})
			} else {
				m.Values = append(m.Values, Named{Name: k, Values: []Node{child}})
			}
		}
	}

	// simplify
	if m.Op == MopEMPTY && hasConditional && len(m.Values) == 1 {
		return m.Values[0], nil
	}
	return m, nil
}

func main() {
	i := 0

	{
		fmt.Println("")
		fmt.Println(`db.inventory.find( { qty: { $gt: 20 } } )`)
		m := M{Values: []Node{
			Named{Name: "qty", Values: []Node{U{Op: UopGT, Value: int64(20)}}},
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
		m := M{Values: []Node{
			Named{Name: "xxx.qty", Values: []Node{U{Op: UopGT, Value: int64(20)}}},
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

	{
		fmt.Println("")
		fmt.Println(`db.inventory.find( { qty: { $gt: 20, $lt: 50  } } )`)
		m := M{
			Values: []Node{
				Named{
					Name: "qty",
					Values: []Node{
						U{Op: UopGT, Value: int64(20)},
						U{Op: UopLT, Value: int64(50)},
					},
				},
			},
		}
		fmt.Println(m)

		vs := map[string]interface{}{}
		json.Unmarshal([]byte(`{ "qty": { "$gt": 20, "$lt": 50  } }`), &vs)
		m2, err := Parse(vs)
		fmt.Printf("ok?=%v, %#+v, err=%+v\n", reflect.DeepEqual(m, m2), m2, err)

		i++
		ioutil.WriteFile(fmt.Sprintf("/tmp/before.%d", i), []byte(spew.Sdump(m)), 0744)
		ioutil.WriteFile(fmt.Sprintf("/tmp/after.%d", i), []byte(spew.Sdump(m2)), 0744)
	}

	{
		fmt.Println("")
		fmt.Println(`
	db.inventory.find( { $and: [
	  {qty: { $gt: 20 }},
	  {qty: { $lt: 50 }},
	] } )`)
		m := M{
			Op: MopAND,
			Values: []Node{
				M{
					Values: []Node{
						Named{
							Name: "qty",
							Values: []Node{
								U{Op: UopGT, Value: int64(20)},
							},
						},
					},
				},
				M{
					Values: []Node{
						Named{
							Name: "qty",
							Values: []Node{
								U{Op: UopLT, Value: int64(50)},
							},
						},
					},
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
		m := M{
			Values: []Node{
				Named{Name: "xxx",
					Values: []Node{
						Named{Name: "qty", Values: []Node{U{Op: UopGT, Value: int64(20)}}},
					},
				},
				Named{Name: "yyy", Values: []Node{U{Op: UopEQ, Value: int64(10)}}},
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
						M{
							Values: []Node{
								Named{Name: "qty", Values: []Node{
									U{Op: UopGT, Value: int64(50)},
									U{Op: UopLT, Value: int64(10)},
								}},
							},
						},
					},
				},
				M{
					Op: MopOR,
					Values: []Node{
						M{
							Values: []Node{Named{Name: "sale", Values: []Node{Bool(true)}}},
						},
						M{
							Values: []Node{Named{Name: "price", Values: []Node{U{Op: UopLT, Value: int64(5)}}}},
						},
					},
				},
			},
		}

		fmt.Println(m)

		vs := map[string]interface{}{}
		json.Unmarshal([]byte(`
	{
		    "$and": [
		        { "$or": [ { "qty": { "$gt": 50, "$lt": 10 } } ] },
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
