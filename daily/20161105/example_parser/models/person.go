package main

import (
	"gopkg.in/mgo.v2/bson"
)

// Person : person model
type Person struct {
	ID   bson.ObjectId `json:"id" bson:"_id"`
	Name string        `json:"name" bson:"name"`
	Age  int           `json:"age" bson:"age"`
}
