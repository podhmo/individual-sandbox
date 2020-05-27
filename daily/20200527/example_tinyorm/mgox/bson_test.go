package mgox

import (
	"fmt"
	"m/miniq"
	"strings"
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

	cases := []struct {
		input *miniq.WhereClause
		want  string
	}{
		{
			input: miniq.Where(
				Status.Compare("$eq", "A"), // todo {"status": "A"}
			),
			want: `{"status":{"$eq":"A"}}`,
		},
		{
			input: miniq.Where(
				Status.Compare("$eq", "A"), // todo {"status": "A"}
				Qty.Compare("$lt", 30),
				ID.CompareN("$in", []bson.ObjectId{bson.ObjectIdHex("5ece879dc54d2d1bec8d12bb")}),
			),
			want: `{"_id":{"$in":[{"$oid":"5ece879dc54d2d1bec8d12bb"}]},"qty":{"$lt":{"$numberLong":30}},"status":{"$eq":"A"}}`,
		},
	}

	for i, c := range cases {
		c := c
		t.Run(fmt.Sprintf("case:%d", i), func(t *testing.T) {
			b, err := bson.MarshalJSON(BSON(c.input))
			if err != nil {
				t.Fatal(err)
			}
			got := string(b)
			if strings.TrimSpace(c.want) != strings.TrimSpace(got) {
				t.Errorf("\nwant\n\t%v\nbut\n\t%v", c.want, got)
			}
		})
	}
}
