package mgox

import (
	"m/miniq"

	"github.com/globalsign/mgo/bson"
)

func BSON(where *miniq.WhereClause) bson.M {
	q := bson.M{}
	for _, v := range where.Value.Values {
		bind(q, v)
	}
	return q
}

func bind(q bson.M, op interface{}) {
	switch op := op.(type) {
	case *miniq.Bop:
		q[op.Left] = bson.M{op.Op: op.Right}
	default:
		panic("hmm")
	}
}
