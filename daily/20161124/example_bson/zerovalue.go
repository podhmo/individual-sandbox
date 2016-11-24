package main

import (
	"log"

	"gopkg.in/mgo.v2/bson"
)

type S struct {
	ID bson.ObjectId `bson:"_id"`
}

func main() {
	s := S{}
	log.Printf("%+v\n", s)
}
