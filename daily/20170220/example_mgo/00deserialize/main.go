package main

import (
	"encoding/json"

	"github.com/davecgh/go-spew/spew"
	"gopkg.in/mgo.v2/bson"
)

type P struct {
	ID   bson.ObjectId `bson:"_id" json:"_id"`
	Name string        `bson:"name" json:"name"`
}

func main() {
	d := `
{
  "_id": "58a26813232d12000110ab73",
  "name": "foo"
}
`
	var p P
	json.Unmarshal([]byte(d), &p)
	spew.Dump(p)

	// (main.P) {
	//  ID: (bson.ObjectId) (len=12) ObjectIdHex("58a26813232d12000110ab73"),
	//  Name: (string) (len=3) "foo"
	// }
}
