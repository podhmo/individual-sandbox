package main

import (
	"log"
	"m/base"

	"gopkg.in/mgo.v2/bson"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	u := &base.UserService{}
	userID := bson.NewObjectId()
	return u.Delete(userID)
}
