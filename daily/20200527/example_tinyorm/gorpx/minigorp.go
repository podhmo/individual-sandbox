package gorpx

import (
	"fmt"
	"m/miniq"

	"github.com/go-gorp/gorp/v3"
)

type Table = miniq.Table

func SelectOne(
	dbmap *gorp.DbMap,
	ob interface{},
	from *miniq.FromClause,
	where *miniq.WhereClause,
) error {
	return dbmap.SelectOne(
		ob,
		fmt.Sprintf("select * %v %v", from, where),
		miniq.Values(where.Value)...,
	)
}
