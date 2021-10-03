package main

import (
	"fmt"
	"log"
	"net/url"

	"github.com/gorilla/schema"
)

func main() {
	decoder := schema.NewDecoder()
	decoder.SetAliasTag("path")

	params, err := url.ParseQuery("")
	if err != nil {
		log.Fatal(err)
	}
	params.Add("id", "")

	type Body struct {
		ID   string `path:"id,required"`
		Name string `path:"name,required"`
	}
	var body Body
	if err := decoder.Decode(&body, params); err != nil {
		log.Printf("404 %+v", err)
	}
	fmt.Printf("%#+v!!\n", body)
}
