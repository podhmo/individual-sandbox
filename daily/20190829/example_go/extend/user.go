package extend

import (
	"fmt"

	"gopkg.in/mgo.v2/bson"
)

// UserService :
type UserService struct {
	InternalFunc func(bson.ObjectId) error
	UseExtend    bool
}

// Delete :
func (u *UserService) Delete(id bson.ObjectId) error {
	if u.UseExtend {
		fmt.Printf("ex delete: %s\n", id.Hex())
		return nil
	}
	return u.InternalFunc(id)
}
