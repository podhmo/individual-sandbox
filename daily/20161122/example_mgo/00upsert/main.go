package main

import (
	"fmt"

	mgo "gopkg.in/mgo.v2"
	"gopkg.in/mgo.v2/bson"
)

// Person :
type Person struct {
	Name string        `bson:"name"`
	ID   bson.ObjectId `bson:"_id"`
}

func run(session *mgo.Session) {
	foo := Person{
		ID:   bson.NewObjectId(),
		Name: "foo",
	}
	bar := Person{
		ID:   bson.NewObjectId(),
		Name: "bar",
	}
	boo := Person{
		ID:   bson.NewObjectId(),
		Name: "boo",
	}
	fmt.Println("initial store data")
	fmt.Println("\tfoo: inserted")
	fmt.Println("\tbar: inserted")
	people := session.DB("test").C("people")
	if err := people.Insert(&foo, &bar); err != nil {
		panic(err)
	}
	var xs []Person
	if err := people.Find(nil).All(&xs); err != nil {
		panic(err)
	}
	fmt.Println(xs)
	genChange := func(m interface{}) mgo.Change {
		return mgo.Change{
			Update:    m,
			Upsert:    true,
			ReturnNew: true,
		}
	}
	fmt.Println("\tfoo.Name: foo -> foo2")
	fmt.Println("\tboo: inserted")

	foo.Name = "foo2"
	if _, err := people.Find(bson.M{"_id": foo.ID}).Apply(genChange(&foo), &foo); err != nil {
		panic(err)
	}
	if _, err := people.Find(bson.M{"_id": boo.ID}).Apply(genChange(&boo), &boo); err != nil {
		panic(err)
	}
	_ = boo
	var ys []Person
	if err := people.Find(nil).All(&ys); err != nil {
		panic(err)
	}
	fmt.Println(ys)
}

const (
	// IsDrop :
	IsDrop = true
)

func main() {
	url := "localhost:27017"
	session, err := mgo.Dial(url)
	if err != nil {
		panic(err)
	}

	session.SetMode(mgo.Monotonic, true)

	// Drop Database
	if IsDrop {
		err = session.DB("test").DropDatabase()
		if err != nil {
			panic(err)
		}
	}
	run(session)
	defer session.Close()
}
