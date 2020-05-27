package mgox

import (
	"github.com/globalsign/mgo/bson"
)

func BSON(where *WhereClause) bson.M {
	q := bson.M{}
	for _, v := range where.Value.Values {
		bind(q, v)
	}
	return q
}

func bind(q bson.M, op interface{}) {
	switch op := op.(type) {
	case *Bop:
		q[op.Left.(string)] = bson.M{op.Op: op.Right}
	default:
		panic("hmm")
	}
}
