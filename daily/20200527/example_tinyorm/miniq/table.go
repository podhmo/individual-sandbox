package miniq

import "fmt"

type tableLike interface {
	TableName() string
}

type Table string

func (t Table) TableName() string {
	return string(t)
}
func (t Table) Join(rhs tableLike, on string) JoinedTable {
	return JoinedTable{lhs: t, rhs: rhs, on: on, joiner: "JOIN"}
}
func (t Table) LeftOuterJoin(rhs tableLike, on string) JoinedTable {
	return JoinedTable{lhs: t, rhs: rhs, on: on, joiner: "LEFT OUTER JOIN"}
}
func (t Table) RightOuterJoin(rhs tableLike, on string) JoinedTable {
	return JoinedTable{lhs: t, rhs: rhs, on: on, joiner: "RIGHT OUTER JOIN"}
}
func (t Table) FullOuterJoin(rhs tableLike, on string) JoinedTable {
	return JoinedTable{lhs: t, rhs: rhs, on: on, joiner: "FULL OUTER JOIN"}
}

type JoinedTable struct {
	lhs    tableLike
	rhs    tableLike
	joiner string
	on     string
}

func (t JoinedTable) TableName() string {
	return fmt.Sprintf("%s %s %s %s",
		t.lhs.TableName(),
		t.joiner,
		t.rhs.TableName(),
		t.on,
	)
}
func (t JoinedTable) Join(rhs tableLike, on string) JoinedTable {
	return JoinedTable{lhs: t, rhs: rhs, joiner: "JOIN"}
}
func (t JoinedTable) LeftOuterJoin(rhs tableLike, on string) JoinedTable {
	return JoinedTable{lhs: t, rhs: rhs, joiner: "LEFT OUTER JOIN"}
}
func (t JoinedTable) RightOuterJoin(rhs tableLike, on string) JoinedTable {
	return JoinedTable{lhs: t, rhs: rhs, joiner: "RIGHT OUTER JOIN"}
}
func (t JoinedTable) FullOuterJoin(rhs tableLike, on string) JoinedTable {
	return JoinedTable{lhs: t, rhs: rhs, joiner: "FULL OUTER JOIN"}
}

func On(lhs, rhs Field) string {
	return fmt.Sprintf("ON %s=%s", lhs.Name(), rhs.Name())
}

func (t Table) Query(
	options ...func(*QueryImpl),
) *QueryImpl {
	q := &QueryImpl{
		FromClause:   From(t),
		SelectClause: Select(STAR),
		WhereClause:  Where(),
	}
	for _, opt := range options {
		opt(q)
	}
	return q
}

func (Table) Select(fields ...Field) func(*QueryImpl) {
	return func(q *QueryImpl) {
		q.SelectClause = Select(fields...)
	}
}

func (Table) Where(ops ...op) func(*QueryImpl) {
	return func(q *QueryImpl) {
		q.WhereClause = Where(ops...)
	}
}

func (Table) From(t tableLike) func(*QueryImpl) {
	return func(q *QueryImpl) {
		q.FromClause = From(t)
	}
}
