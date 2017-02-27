package parse

import (
	"fmt"
	"testing"

	"github.com/davecgh/go-spew/spew"
	"github.com/foo/bee/models"
	"github.com/go-openapi/swag"
)

func TestParse(t *testing.T) {
	var ob models.Ob
	if err := swag.ReadJSON([]byte(`{"exclude": true}`), &ob); err != nil {
		t.Fatal(err)
	}
	spew.Dump(ob)
}

func TestDump(t *testing.T) {
	exclude := models.Exclude(true)
	ob := models.Ob{Exclude: &exclude}
	b, err := swag.WriteJSON(&ob)
	if err != nil {
		t.Fatal(err)
	}
	fmt.Println(string(b))
}
