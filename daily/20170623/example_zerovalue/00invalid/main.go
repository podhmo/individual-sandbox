package main

import (
	"fmt"
	"log"

	"gopkg.in/mgo.v2/bson"
)

// O :
type O struct {
	ID bson.ObjectId `bson:"_id"`
}

func main() {
	{
		o := O{ID: bson.NewObjectId()}
		fmt.Println("initialized", o)
		b, err := bson.Marshal(o)
		if err != nil {
			log.Fatal("ng", err)
		}
		fmt.Println("ok", b)
	}
	fmt.Println("----------------------------------------")
	{
		// error is occured
		var o O
		fmt.Println("zero", o)
		b, err := bson.Marshal(o)
		if err != nil {
			log.Fatal(err)
		}
		fmt.Println("ok", b)
	}
}
