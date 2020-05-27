package tinyorm

import (
	"fmt"
	"strings"
)

type Uop struct {
	OP    string
	Value interface{} // with converter?

	WithoutParen bool
}

func (q *Uop) Format(s fmt.State, c rune) {
	if q.WithoutParen {
		fmt.Fprintf(s, "%s %v", q.OP, q.Value)
		return
	}
	fmt.Fprintf(s, "(%s %v)", q.OP, q.Value)
}

type Bop struct {
	OP    string
	Left  interface{}
	Right interface{}

	WithoutParen bool
}

func (q *Bop) Format(s fmt.State, c rune) {
	if q.WithoutParen {
		fmt.Fprintf(s, "%v %s %v", q.Left, q.OP, q.Right)
		return
	}
	fmt.Fprintf(s, "(%v %s %v)", q.Left, q.OP, q.Right)
}

type Mop struct {
	OP     string
	Values []interface{}

	WithoutParen bool
}

func (q *Mop) Format(s fmt.State, c rune) {
	switch len(q.Values) {
	case 0:
		return
	case 1:
		fmt.Fprintf(s, "%v", q.Values[0])
		return
	}

	var b strings.Builder
	if !q.WithoutParen {
		b.WriteString("(")
	}

	op := " " + q.OP + " "
	for i := range q.Values {
		b.WriteString("%v")
		if i < len(q.Values)-1 {
			b.WriteString(op)
		}
	}

	if !q.WithoutParen {
		b.WriteString(")")
	}
	fmt.Fprintf(s, b.String(), q.Values...)
}

func replace(op interface{}, defaultVal string) interface{} {
	if op == nil {
		return nil
	}
	switch op := op.(type) {
	case *Uop:
		return &Uop{
			OP:           op.OP,
			Value:        replace(op.Value, defaultVal),
			WithoutParen: op.WithoutParen,
		}
	case *Bop:
		return &Bop{
			OP:           op.OP,
			Left:         replace(op.Left, defaultVal),
			Right:        replace(op.Right, defaultVal),
			WithoutParen: op.WithoutParen,
		}
	case *Mop:
		values := make([]interface{}, len(op.Values))
		for i, val := range op.Values {
			values[i] = replace(val, defaultVal)
		}
		return &Mop{
			OP:           op.OP,
			Values:       values,
			WithoutParen: op.WithoutParen,
		}
	default:
		return defaultVal
	}
}
func Replace(op op, val string) op {
	switch v := replace(op, val).(type) {
	case *Uop:
		return v
	case *Bop:
		return v
	case *Mop:
		return v
	default:
		panic(fmt.Sprintf("unexpected return type %T", v))
	}
}

func Values(op interface{}) []interface{} {
	var r []interface{}
	switch op := op.(type) {
	case *Uop:
		r = append(r, Values(op.Value)...)
	case *Bop:
		r = append(r, Values(op.Left)...)
		r = append(r, Values(op.Right)...)
	case *Mop:
		for _, v := range op.Values {
			r = append(r, Values(v)...)
		}
	default:
		r = append(r, op)
	}
	return r
}

type op interface {
	op()
}

func (*Uop) op() {}
func (*Bop) op() {}
func (*Mop) op() {}
