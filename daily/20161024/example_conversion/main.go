package main

import (
	"encoding/json"
	"fmt"

	"github.com/podhmo/hmm/convert"
	"github.com/podhmo/hmm/model"
	"gopkg.in/mgo.v2/bson"
)

func main() {
	{
		page := &model.Page{
			Id:       bson.NewObjectId(),
			Path:     "/index",
			Title:    "index page",
			PathHash: "#",
		}
		defPage, err := convert.FromModelPageToDefPage(page)
		if err != nil {
			panic(err)
		}
		b, err := json.MarshalIndent(defPage, "", "  ")
		if err != nil {
			panic(err)
		}
		fmt.Println(string(b))
	}
	{
		user := &model.User{
			Id:     bson.NewObjectId(),
			Name:   "Foo",
			Gender: model.GenderFemale,
			Group: &model.Group{
				Id:   bson.NewObjectId(),
				Name: "G",
			},
			Skills: []model.Skill{
				{Name: "a"}, {Name: "b"}, {Name: "c"},
			},
			Skills2: []model.Skill{
				{Name: "x"}, {Name: "y"}, {Name: "z"},
			},
		}
		defUser, err := convert.FromModelUserToDefUser(user)
		if err != nil {
			panic(err)
		}
		b, err := json.MarshalIndent(defUser, "", "  ")
		if err != nil {
			panic(err)
		}
		fmt.Println(string(b))
	}
}
