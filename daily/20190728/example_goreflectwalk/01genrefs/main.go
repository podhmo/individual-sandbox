package main

import (
	"fmt"
	"log"
	"m/reflectwalk"
	"strings"
	"time"

	"github.com/k0kubun/pp"
	"gopkg.in/mgo.v2/bson"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("+%v", err)
	}
}

// Dummy :
type Dummy struct {
	ID        bson.ObjectId  `json:"id" bson:"_id"`
	ParentID  *bson.ObjectId `json:"parentId" bson:"parentId"`
	ParentID2 *bson.ObjectId `json:"parentId2" bson:"parentId2"`
	Name      string         `json:"name" bson:"name"`
	Now       time.Time      `json:"now" bson:"age"`
}

func run() error {
	parentID := bson.NewObjectId()
	dummy := &Dummy{
		ID:       bson.NewObjectId(),
		ParentID: &parentID,
		Name:     "foo",
		Now:      time.Now(),
	}
	refs := map[string]interface{}{}

	err := reflectwalk.ReflectWalk(dummy, func(path []string, v interface{}) {
		pp.Println("!!", fmt.Sprintf("%T", v), v)

		switch v := v.(type) {
		case bson.ObjectId:
			refs[fmt.Sprintf("#/%s", strings.Join(path, "/"))] = v.Hex()
		case time.Time:
			refs[fmt.Sprintf("#/%s", strings.Join(path, "/"))] = v
		}
	})
	if err != nil {
		return err
	}
	pp.Println(dummy)
	pp.Println(refs)
	return nil
}
