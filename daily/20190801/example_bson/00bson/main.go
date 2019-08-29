package main

import (
	"fmt"
	"log"

	"github.com/pkg/errors"
	"gopkg.in/mgo.v2/bson"
)

type person struct {
	ID   bson.ObjectId `bson:"_id"`
	Name string        `bson:"name"`
	Age  int           `bson:"age"`
}

type wperson struct {
	ID     bson.ObjectId `bson:"-"`
	Person person       `bson:",inline"'`
}

func main() {
	p := &person{
		ID:   bson.ObjectIdHex("5c75e90b1626e90001cdeb1d"),
		Name: "foo",
		Age:  20,
	}

	if err := run(p); err != nil {
		log.Fatal(err)
	}
	if err := run(wperson{Person: *p}); err != nil {
		log.Fatal(err)
	}
}

func run(p interface{}) error {
	body, err := bson.Marshal(p)
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
	return nil
}
