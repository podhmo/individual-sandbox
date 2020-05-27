package miniq

import (
	"fmt"
)

type QueryImpl struct {
	SelectClause *SelectClause
	FromClause   *FromClause
	WhereClause  *WhereClause
}

func Query(
	selectClause *SelectClause,
	fromClause *FromClause,
	whereClause *WhereClause,
) *QueryImpl {
	return &QueryImpl{
		SelectClause: selectClause,
		FromClause:   fromClause,
		WhereClause:  whereClause,
	}
}

func (q *QueryImpl) Do(
	fn func(ob interface{}, stmt string, args ...interface{}) error,
	ob interface{},
) error {
	return fn(
		ob,
		fmt.Sprintf("%v %v %v", q.SelectClause, q.FromClause, q.WhereClause),
		Values(q.WhereClause.Value)...,
	)
}
func (q *QueryImpl) DoWithValue(
	fn func(ob interface{}, stmt string, args ...interface{}) (interface{}, error),
	ob interface{},
) (interface{}, error) {
	return fn(
		ob,
		fmt.Sprintf("%v %v %v", q.SelectClause, q.FromClause, q.WhereClause),
		Values(q.WhereClause.Value)...,
	)
}

func (q *QueryImpl) DoWithValues(
	fn func(ob interface{}, stmt string, args ...interface{}) ([]interface{}, error),
	ob interface{},
) ([]interface{}, error) {
	return fn(
		ob,
		fmt.Sprintf("%v %v %v", q.SelectClause, q.FromClause, q.WhereClause),
		Values(q.WhereClause.Value)...,
	)
}
