package model

import "gopkg.in/mgo.v2/bson"

type Page struct {
	Id       bson.ObjectId `bson:"_id" json:"id"`
	Path     string        `bson:"path" json:"path"`
	PathHash string        `bson:"pathHash" json:"pathHash"`
	Title    string        `bson:"title" json:"title"`
}

type Gender string

const (
	GenderFemale = Gender("xx")
	GenderMALE   = Gender("xy")
)

type User struct {
	Id      bson.ObjectId `bson:"_id" json:"id"`
	Name    string        `bson:"name" json:"name"`
	Gender  Gender        `bson:"gender" json:"gender"`
	GroupID string        `bson:"groupId" json:"groupId"`

	Group *Group `bson:"-" json:"group"`
}
