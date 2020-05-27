package miniq

import (
	"fmt"
	"strings"
)

func Where(ops ...op) *WhereClause {
	values := make([]interface{}, len(ops))
	for i, op := range ops {
		values[i] = op
	}

	value := And(values...)
	value.WithoutParen = true
	return &WhereClause{Prefix: "WHERE", Value: value}
}

type WhereClause struct {
	Prefix string
	Value  *Mop
}

func (q *WhereClause) String() string {
	if len(q.Value.Values) == 0 {
		return ""
	}

	value := Replace(q.Value, "")
	return fmt.Sprintf("%s %v", q.Prefix, value)
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

func (q *FromClause) String() string {
	return fmt.Sprintf("%s %s", q.Prefix, q.Table)
}

func Select(fields ...Field) *SelectClause {
	return &SelectClause{Prefix: "SELECT", Fields: fields}
}

type SelectClause struct {
	Prefix string
	Fields []Field
}

func (q *SelectClause) String() string {
	names := make([]string, len(q.Fields))
	for i, f := range q.Fields {
		names[i] = f.Name()
	}
	return fmt.Sprintf("%s %s", q.Prefix, strings.Join(names, ", "))
}

type Table string
