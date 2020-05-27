package mgox

import (
	"fmt"

	"github.com/globalsign/mgo/bson"
)

type Field interface {
	Name() string
}
type As struct {
	NewName string
	Field   Field
}

func (as As) Name() string {
	return fmt.Sprintf("%s as %s", as.Field.Name(), as.NewName)
}

type Int64Field string

func (f Int64Field) Name() string {
	return string(f)
}
func (f Int64Field) As(name string) As {
	return As{NewName: name, Field: f}
}
func (f Int64Field) Compare(op string, value int64) *Bop {
	return &Bop{
		Op:           op,
		Left:         f.Name(),
		Right:        value,
		WithoutParen: true,
	}
}

type StringField string

func (f StringField) Name() string {
	return string(f)
}
func (f StringField) As(name string) As {
	return As{NewName: name, Field: f}
}
func (f StringField) Compare(op string, value string) *Bop {
	return &Bop{
		Op:           op,
		Left:         f.Name(),
		Right:        value,
		WithoutParen: true,
	}
}

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
