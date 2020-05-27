package miniq

import (
	"fmt"
)

type WhereClause struct {
	Prefix string
	Value  op
}

func Where(ops ...interface{}) *WhereClause {
	value := And(ops...)
	value.WithoutParen = true
	return &WhereClause{Prefix: "WHERE", Value: value}
}

func (q *WhereClause) Format(s fmt.State, c rune) {
	value := Replace(q.Value, "")
	fmt.Fprintf(s, "%s %v", q.Prefix, value)
}

type FromClause struct {
	Prefix string
	Table  Table
}

func From(table Table) *FromClause {
	return &FromClause{
		Prefix: "FROM",
		Table:  table,
	}
}

func (q *FromClause) Format(s fmt.State, c rune) {
	fmt.Fprintf(s, "%s %s", q.Prefix, q.Table)
}

type Table string

func NewInt64Field(name string) func(format string, value int64) *Uop {
	return func(format string, value int64) *Uop {
		return &Uop{
			Op:           fmt.Sprintf(format, name),
			Value:        value,
			WithoutParen: true,
		}
	}
}

func NewStringField(name string) func(format string, value string) *Uop {
	return func(format string, value string) *Uop {
		return &Uop{
			Op:           fmt.Sprintf(format, name),
			Value:        value,
			WithoutParen: true,
		}
	}
}
