package main

import (
	"encoding/json"
	"os"

	"gopkg.in/mgo.v2/bson"
)

type s struct {
	ID bson.ObjectId `bson:"_id" json:"id"`
}

func main() {
	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	encoder.Encode(s{ID: bson.NewObjectId()})
}
