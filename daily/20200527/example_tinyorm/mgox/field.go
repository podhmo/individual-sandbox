package mgox

import (
	"m/miniq"

	"github.com/globalsign/mgo/bson"
)

type Bop = miniq.Bop

type As = miniq.As

type Int64Field = miniq.Int64Field
type StringField = miniq.StringField

type ObjectIdField string

func (f ObjectIdField) Name() string {
	return string(f)
}
func (f ObjectIdField) As(name string) As {
	return As{NewName: name, Field: f}
}
func (f ObjectIdField) Compare(op string, value bson.ObjectId) *Bop {
	return &Bop{
		Op:           op,
		Left:         f.Name(),
		Right:        value,
		WithoutParen: true,
	}
}
func (f ObjectIdField) CompareN(op string, value []bson.ObjectId) *Bop {
	return &Bop{
		Op:           op,
		Left:         f.Name(),
		Right:        value,
		WithoutParen: true,
	}
}
