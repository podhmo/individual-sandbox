package model

import "gopkg.in/mgo.v2/bson"

type Page struct {
	Id       bson.ObjectId `bson:"_id" json:"id"`
	Path     string        `bson:"path" json:"path"`
	PathHash string        `bson:"pathHash" json:"pathHash"`
	Title    string        `bson:"title" json:"title"`
}
