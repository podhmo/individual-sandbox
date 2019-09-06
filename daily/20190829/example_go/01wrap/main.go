package main

import (
	"log"
	"m/base"
	"m/extend"

	"github.com/pkg/errors"
	"gopkg.in/mgo.v2/bson"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	{
		useExtend := false
		u := &extend.UserService{
			InternalFunc: new(base.UserService).Delete,
			UseExtend:    useExtend,
		}
		userID := bson.NewObjectId()
		if err := u.Delete(userID); err != nil {
			errors.WithMessagef(err, "use extend %b", useExtend)
		}
	}
	{
		useExtend := true
		u := &extend.UserService{
			InternalFunc: new(base.UserService).Delete,
			UseExtend:    useExtend,
		}
		userID := bson.NewObjectId()
		if err := u.Delete(userID); err != nil {
			errors.WithMessagef(err, "use extend %b", useExtend)
		}
	}
	return nil
}
