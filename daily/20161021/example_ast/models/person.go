package models

import (
	"gopkg.in/mgo.v2/bson"
)

// PersonGender : gender
type PersonGender string

// PersonGender : constants
const (
	PersonGenderFemale = PersonGender("female")
	PersonGendermale   = PersonGender("male")
)
const PersonGenderUnknown = PersonGender("unknown")

// Person : person model
type Person struct {
	Id      bson.ObjectId `json:"id" bson:"_id"`
	Name    string        `json:"name"`
	Age     int           `json:"age"`
	Gender  PersonGender  `json:"gender"`
	GroupId *string       `json:"groupId,omitempty"`

	Group  *Group `json:"-"`
	Modify func(other *Person) (*Person, error)
}

/*
struct {
  comment: "",
  fields:  {
    <name>: <item>
  }
}
<item> : {type, tags} | {struct, tags}
*/
