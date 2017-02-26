package models

import (
	"testing"

	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
)

func Test(t *testing.T) {
	t.Run("strfmt is ok", func(t *testing.T) {
		var ob strfmt.Date
		if err := swag.ReadJSON([]byte(`"2017-01-01"`), &ob); err != nil {
			t.Fatal(err)
		}
	})
	t.Run("this is ng", func(t *testing.T) {
		var ob Mydate
		if err := swag.ReadJSON([]byte(`"2017-01-01"`), &ob); err != nil {
			t.Fatal(err)
		}
	})
}
