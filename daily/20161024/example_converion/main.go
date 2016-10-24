package main

import (
	"fmt"

	"encoding/json"

	"github.com/podhmo/hmm/convert"
	"github.com/podhmo/hmm/model"
)

func main() {
	page := &model.Page{Id: "1", Path: "/index", Title: "index page", PathHash: "#"}
	defPage, err := convert.ConvertFromPage(page)
	if err != nil {
		panic(err)
	}
	b, err := json.Marshal(defPage)
	if err != nil {
		panic(err)
	}
	fmt.Println(string(b))
}
