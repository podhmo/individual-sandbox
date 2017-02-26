package bug

import (
	"testing"
	"github.com/foo/boo/models"
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
		var ob models.DateTextRaw
		if err := swag.ReadJSON([]byte(`"2017-01-01"`), &ob); err != nil {
			t.Fatal(err)
		}
	})
}
