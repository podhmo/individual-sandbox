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
	Skills  []Skill       `bson:"skills" json:"skills"`
	Skills2 []Skill       `bson:"skills2" json:"skills2"`
	Skills3 []*Skill       `bson:"skills3" json:"skills3"`
	Group   *Group        `bson:"-" json:"group"`
}

type Skill struct {
	Name string `bson:"name" json:"name"`
}
