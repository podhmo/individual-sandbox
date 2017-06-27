package main

import (
	"fmt"
	"log"

	"github.com/k0kubun/pp"
	"gopkg.in/mgo.v2/bson"
)

// T :
type T int

//
const (
	TX = T(0)
	TY = T(1)
)

// A :
type A struct {
	T T
	X X
	Y Y
}

// X :
type X struct {
	ID bson.ObjectId `bson:"_id"`
}

// Y :
type Y struct {
	V int `bson:"v"`
}

func main() {
	{
		// error is occured
		a := A{T: TX, Y: Y{V: 10}}
		pp.Println(a)
		b, err := bson.Marshal(a)
		if err != nil {
			log.Fatal(err)
		}
		fmt.Println("ok", b)
	}
}
