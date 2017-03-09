package main

import (
	"fmt"
	"log"

	"gopkg.in/mgo.v2/bson"
)

// P :
type P struct {
	ID   bson.ObjectId `bson:"_id"`
	Name string        `bson:"name"`
}

func main() {
	p := P{Name: "foo"}
	// p := P{Name: "foo", ID: bson.NewObjectId()}

	b, err := bson.Marshal(&p)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("create -> marshal -> unmarshal")
	var p2 P
	if err := bson.Unmarshal(b, &p2); err != nil {
		log.Fatal(err)
	}
	log.Printf("%v", p2)
}
