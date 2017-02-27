package parse

import (
	"fmt"
	"testing"

	"github.com/davecgh/go-spew/spew"
	"github.com/foo/bee/models"
	"github.com/go-openapi/swag"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestParse(t *testing.T) {
	var ob models.Ob
	if err := swag.ReadJSON([]byte(`{"exclude": true}`), &ob); err != nil {
		t.Fatal(err)
	}
	spew.Dump(ob)
}

func TestParseRaw(t *testing.T) {
	t.Run("True", func(t *testing.T) {
		r, err := swag.ConvertBool(`True`)
		require.NoError(t, err)
		assert.Equal(t, true, r)
	})
	t.Run("False", func(t *testing.T) {
		r, err := swag.ConvertBool(`False`)
		require.NoError(t, err)
		assert.Equal(t, false, r)
	})
	t.Run("true", func(t *testing.T) {
		r, err := swag.ConvertBool(`true`)
		require.NoError(t, err)
		assert.Equal(t, true, r)
	})
	t.Run("false", func(t *testing.T) {
		r, err := swag.ConvertBool(`false`)
		require.NoError(t, err)
		assert.Equal(t, false, r)
	})
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
