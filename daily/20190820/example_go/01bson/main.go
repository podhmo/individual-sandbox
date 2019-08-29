package main

import (
	"fmt"

	"github.com/k0kubun/pp"
	"gopkg.in/mgo.v2/bson"
)

type P struct {
	*Q `bson:",inline"`
}
type Q struct {
	X int `json:"x" bson:"x"`
	Y int `json:"y" bson:"y"`
}

func main() {
	p := &P{Q: &Q{X: 10, Y: 20}}
	b, _ := bson.MarshalJSON(p)
	fmt.Println(string(b))

	{
		var p P
		b := []byte(`{"x": 10, "y": 20}`)
		bson.UnmarshalJSON(b, &p)
		pp.Println(p)
	}

}
