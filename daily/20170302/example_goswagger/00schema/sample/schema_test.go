package sample

import (
	"testing"

	"github.com/foo/boah/models"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestSchemaValidate(t *testing.T) {
	registry := strfmt.NewFormats()
	t.Run("ok", func(t *testing.T) {
		var ob models.Person
		payload := []byte(`{"name": "foo"}`)
		require.NoError(t, swag.ReadJSON(payload, &ob))
		assert.NoError(t, ob.Validate(registry))
	})
	t.Run("ng validate", func(t *testing.T) {
		var ob models.Person
		payload := []byte(`{"age": 100}`)
		require.NoError(t, swag.ReadJSON(payload, &ob))
		assert.Error(t, ob.Validate(registry), "name is at least 1 chars long")
	})
	t.Run("ng load", func(t *testing.T) {
		var ob models.Person
		payload := []byte(`{"age": "100"}`)
		assert.Error(t, swag.ReadJSON(payload, &ob), "age is must be int")
	})
}
