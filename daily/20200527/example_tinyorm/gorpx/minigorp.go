package gorpx

import (
	"fmt"
	"m/miniq"

	"github.com/go-gorp/gorp/v3"
)

type Table = miniq.Table

type DbMapx struct {
	DbMap *gorp.DbMap
}

func (x *DbMapx) SelectOne(
	ob interface{},
	selectClause *miniq.SelectClause,
	fromClause *miniq.FromClause,
	whereClause *miniq.WhereClause,
) error {
	return x.DbMap.SelectOne(
		ob,
		fmt.Sprintf("%v %v %v", selectClause, fromClause, whereClause),
		miniq.Values(whereClause.Value)...,
	)
}

func (x *DbMapx) Select(
	ob interface{},
	selectClause *miniq.SelectClause,
	fromClause *miniq.FromClause,
	whereClause *miniq.WhereClause,
) (interface{}, error) {
	return x.DbMap.Select(
		ob,
		fmt.Sprintf("%v %v %v", selectClause, fromClause, whereClause),
		miniq.Values(whereClause.Value)...,
	)
}
