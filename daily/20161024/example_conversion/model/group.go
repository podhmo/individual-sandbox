package model

import "gopkg.in/mgo.v2/bson"

type Group struct {
	Id   bson.ObjectId `bson:"_id" json:"id"`
	Name string        `bson:"name" json:"name"`
}
