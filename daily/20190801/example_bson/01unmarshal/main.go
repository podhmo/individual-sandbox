package main

import (
	"fmt"
	"log"

	"github.com/k0kubun/pp"
	"github.com/pkg/errors"
	"gopkg.in/mgo.v2/bson"
)

type person struct {
	ID   bson.ObjectId `bson:"_id"`
	Name string        `bson:"name"`
	Age  int           `bson:"age"`
}

type me struct {
	ID      bson.ObjectId `bson:"_id"`
	Me      *person       `bson:"me"`
	Parents []*person     `bson:"parents"`
}

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

func run() error {
	p := &person{
		ID:   bson.ObjectIdHex("5c75e90b1626e90001cdeb1d"),
		Name: "foo",
		Age:  20,
	}
	me := &me{
		ID:      bson.ObjectIdHex("5c75e90b1626e90001cdeb1a"),
		Me:      p,
		Parents: []*person{p, p},
	}

	// bson.MarshalJSON, bson.UnmarshalJSON
	body, err := bson.Marshal(me)
	if err != nil {
		return errors.Wrap(err, "marshal")
	}
	var m bson.M
	err = bson.Unmarshal(body, &m)
	if err != nil {
		return errors.Wrap(err, "unmarshal")
	}

	fmt.Println(body)
	fmt.Println(m)
	pp.Println(m)
	return nil
}
