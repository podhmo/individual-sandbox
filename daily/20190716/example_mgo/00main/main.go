package main

import (
	"fmt"

	"github.com/globalsign/mgo"
	"gopkg.in/mgo.v2/bson"
)

func main() {
	mgo.SetDebug(true)
	fmt.Println(bson.M{})
}
