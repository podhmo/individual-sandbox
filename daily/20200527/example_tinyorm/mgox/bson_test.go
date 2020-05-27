package mgox

import (
	"fmt"
	"testing"

	"github.com/globalsign/mgo/bson"
)

var (
	ID     = ObjectIdField("_id")
	Qty    = Int64Field("qty")
	Status = StringField("status")
)

// https://docs.mongodb.com/manual/tutorial/query-documents/

func Test(t *testing.T) {

	where := Where(
		Status.Compare("$eq", "A"), // todo {"status": "A"}
		Qty.Compare("$lt", 30),
		ID.CompareN("$in", []bson.ObjectId{bson.ObjectIdHex("5ece879dc54d2d1bec8d12bb")}),
	)
	got := BSON(where)
	b, err := bson.MarshalJSON(got)
	if err != nil {
		t.Fatal(err)
	}
	fmt.Println(got, string(b))
}
