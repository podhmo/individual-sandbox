package main

import (
	"fmt"

	"gopkg.in/mgo.v2/bson"
)

type person struct {
	ID     bson.ObjectId `bson:"_id"`
	Name   string        `bson:"name"`
	Age    string        `bson:"age"`
	Skills []skill       `bson:"skills"`
	Father *person       `bson:"father,omitempty"`
	Mother *person       `bson:"mother,omitempty"`
}

type skill struct {
	Name string `bson:"name"`
}

type personT struct {
	ID     string
	Name   string
	Age    string
	Father *personT
	Mother *personT
}

var PERSON personT

func init() {
	PERSON = personT{ID: "_id", Name: "name", Age: "age"}
}

func main() {
	fmt.Println(bson.M{PERSON.ID: bson.NewObjectId()})
}
