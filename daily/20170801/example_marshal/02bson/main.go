package main

import (
	"fmt"
	"log"

	"gopkg.in/mgo.v2/bson"
)

type P struct {
	Value int `bson:"value"`
}

type Empty struct {
}


func main() {
    b, err := bson.Marshal(&Empty{})
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println("input:", b)

	var ob P
	if err := bson.Unmarshal(b, &ob); err != nil {
		log.Fatal(err)
	}
	fmt.Println(&ob)
}
