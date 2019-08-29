package base

import (
	"fmt"

	"gopkg.in/mgo.v2/bson"
)

// UserService :
type UserService struct {
}

// Delete :
func (u *UserService) Delete(id bson.ObjectId) error {
	fmt.Printf("delete: %s\n", id.Hex())
	return nil
}
