package main

import (
	"encoding/json"
	"fmt"

	"github.com/podhmo/hmm/convert"
	"github.com/podhmo/hmm/model"
	"gopkg.in/mgo.v2/bson"
)

func main() {
	page := &model.Page{Id: bson.NewObjectId(), Path: "/index", Title: "index page", PathHash: "#"}
	defPage, err := convert.ConvertFromModelPage(page)
	if err != nil {
		panic(err)
	}
	b, err := json.Marshal(defPage)
	if err != nil {
		panic(err)
	}
	fmt.Println(string(b))
}
