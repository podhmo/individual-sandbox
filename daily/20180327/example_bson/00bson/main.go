package main

import (
	"log"

	"github.com/globalsign/mgo/bson"
)

// Status :
type Status int

const (
	ok Status = iota
	ng
)

// Job :
type Job struct {
	ID     bson.ObjectId `bson:"_id"`
	Status Status        `bson:"status"`
}

func main() {
	job := Job{}
	_, err := bson.Marshal(job)
	if err != nil {
		log.Fatal(err)
	}
}
