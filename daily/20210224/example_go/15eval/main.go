package main

import (
	"encoding/json"
	"fmt"
	"log"
	"strconv"
	"strings"
	_ "unsafe"
)

// TODO: conflict check

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

	UopRegex = "$regex"
)

type U struct {
	Op    Uop
	Value interface{} // string, int64, float64, bool
}

func (u U) node() {}

type Mop = string

const (
	MopAND   Mop = "$and"
	MopNOT       = "$not"
	MopNOR       = "$nor"
	MopOR        = "$or"
	MopEMPTY     = ""
)

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
		return M{}, fmt.Errorf("parse: %w, input=%+v", err, vs)
	}
	return node.(M), nil
}
func ParseString(q string) (M, error) {
	m := map[string]interface{}{}
	if err := json.Unmarshal([]byte(q), &m); err != nil {
		return M{}, fmt.Errorf("decode JSON: %w, input=%q", err, q)
	}
	return Parse(m)
}

func parse(val interface{}, path []string) (Node, error) {
	switch val := val.(type) {
	case []interface{}:
		return parseArray(val, path)
	case map[string]interface{}:
		return parseMap(val, path)
	case bool:
		return Bool(val), nil
	case int64, string, float64:
		return U{Op: UopEQ, Value: val}, nil
	default:
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
		case UopEQ, UopGT, UopGTE, UopIN, UopLT, UopLTE, UopNE, UopNIN, UopRegex: // U
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
			// TODO: conflict check e.g. { "$lt": 30, "$lt": 50 }
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

func Transform(m M) (string, error) {
	var args []string
	for _, v := range m.Values {
		switch v := v.(type) {
		case Named:
			// TODO: Named.Named
			var subs []string
			for _, x := range v.Values {
				switch x := x.(type) {
				case U:
					subs = append(subs, fmt.Sprintf("%s %s", QuoteName(v.Name), ConvertUop(x.Op, x.Value)))
				default:
					subs = append(subs, fmt.Sprintf("%s ??%v??", QuoteName(v.Name), ConvertValue(x)))
				}
			}
			args = append(args, fmt.Sprintf("%s", strings.Join(subs, " AND ")))
		case M:
			child, err := Transform(v)
			if err != nil {
				return "", err
			}

			// TODO: simplify
			args = append(args, fmt.Sprintf("( %s )", child))
		default:
			return "", fmt.Errorf("unexpected input: %#+v", v)
		}
	}

	op := m.Op
	if op == MopEMPTY {
		op = MopAND
	}
	return strings.Join(args, fmt.Sprintf(" %s ", ConvertMop(op))), nil
}

func QuoteName(name string) string {
	return fmt.Sprintf("`%s`", name)
}
func ConvertMop(op Mop) string {
	switch op {
	case MopAND:
		return "AND"
	case MopNOT:
		return "NOT"
	case MopNOR:
		return "NOT OR" // xxx
	case MopOR:
		return "OR"
	default:
		return fmt.Sprintf("<UNKNOWN Mop %q>", op)
	}
}

func ConvertUop(op Uop, v interface{}) string {
	switch op {
	case UopEQ:
		return "=" + " " + ConvertValue(v)
	case UopGT:
		return ">" + " " + ConvertValue(v)
	case UopGTE:
		return ">=" + " " + ConvertValue(v)
	case UopIN:
		return "in" + " " + ConvertValue(v)
	case UopLT:
		return "<" + " " + ConvertValue(v)
	case UopLTE:
		return "<=" + " " + ConvertValue(v)
	case UopNE:
		return "!=" + " " + ConvertValue(v)
	case UopNIN:
		return "not in" + " " + ConvertValue(v)
	case UopRegex:
		return fmt.Sprintf("LIKE /%v/", v)
	default:
		return fmt.Sprintf("<UNKNOWN Uop %q> %s", op, v)
	}
}

//go:linkname strconv_quotewWith strconv.quoteWith
func strconv_quotewWith(s string, quote byte, ASCIIonly, graphicOnly bool) string

func ConvertValue(v interface{}) string {
	switch v := v.(type) {
	case int64:
		return strconv.FormatInt(v, 10)
	case float64, bool:
		return fmt.Sprintf("%v", v) // xxx
	case string:
		return strconv_quotewWith(v, '\'', false, false)
	default:
		return fmt.Sprintf("<UNKNOWN Value %q>", v)
	}
}
func main() {
	run := func(q string) {
		fmt.Println("")
		fmt.Println(q)
		m, err := ParseString(q)
		if err != nil {
			log.Fatalf("input=%q, err %+v", q, err)
		}
		where, err := Transform(m)
		if err != nil {
			log.Fatalf("m=%q, err %+v", m, err)
		}
		fmt.Println("WHERE", where)
	}

	{
		// SELECT * FROM inventory WHERE qty < 30
		q := `{"qty": {"$gt": 30}}`
		run(q)
	}

	{
		// SELECT * FROM inventory WHERE qty > 30 AND qty < 50
		q := `{"qty": {"$gt": 30, "$lt": 50}}`
		run(q)
	}
	{
		// SELECT * FROM inventory WHERE status = "A" AND qty < 30
		q := `{"status": "A", "qty": {"$lt": 30}}`
		run(q)
	}

	{
		// SELECT * FROM inventory WHERE status = "A" OR (qty > 30 AND qty < 50)
		q := `{ "$or": [ { "status": "A" }, { "qty": { "$gt": 30, "$lt": 50 } } ] }`
		run(q)
	}

	{
		// SELECT * FROM inventory WHERE status = "A" AND ( qty < 30 OR item LIKE "p%")
		q := `{ "status": "A", "$or": [{ "qty": { "$lt": 30}}, {"item": {"$regex": "^p"}}] }`
		run(q)
	}
}

// examples
// - https://docs.mongodb.com/manual/tutorial/query-documents/
