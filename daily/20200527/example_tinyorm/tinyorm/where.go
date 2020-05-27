package tinyorm

import (
	"fmt"
)

func And(values ...interface{}) *Mop {
	return &Mop{OP: "AND", Values: values}
}
func Or(values ...interface{}) *Mop {
	return &Mop{OP: "OR", Values: values}
}
func Not(value interface{}) *Uop {
	return &Uop{OP: "NOT", Value: value}
}

type Clause struct {
	Prefix string
	Value  op
}

func Where(ops ...interface{}) *Clause {
	value := And(ops...)
	value.WithoutParen = true
	return &Clause{Prefix: "WHERE", Value: value}
}

func (q *Clause) Format(s fmt.State, c rune) {
	value := Replace(q.Value, "")
	fmt.Fprintf(s, "%s %v", q.Prefix, value)
}

// TODO: rename
func NewInt64Condition(name string) func(format string, value int64) *Uop {
	return func(format string, value int64) *Uop {
		return &Uop{
			OP:           fmt.Sprintf(format, name),
			Value:        value,
			WithoutParen: true,
		}
	}
}

func NewStringCondition(name string) func(format string, value string) *Uop {
	return func(format string, value string) *Uop {
		return &Uop{
			OP:           fmt.Sprintf(format, name),
			Value:        value,
			WithoutParen: true,
		}
	}
}
