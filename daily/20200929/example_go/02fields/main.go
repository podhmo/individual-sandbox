package main

import (
	"m/field"

	"github.com/k0kubun/pp"
)

// A Field interface returns a field descriptor for vertex fields/properties.
// The usage for the interface is as follows:
//
//	func (T) Fields() []Field {
//		return []Field{
//			field.Int("int"),
//		}
//	}
//
type Field interface {
	Descriptor() *field.Descriptor
}

// User holds the schema definition for the User entity.
type User struct {
}

// Fields of the User.
func (User) Fields() []Field {
	return []Field{
		field.Int("age").
			Positive(),
		field.String("name").
			Default("unknown"),
	}
}

func main() {
	var u User
	pp.Println(u.Fields())
}
