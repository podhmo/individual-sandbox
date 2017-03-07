package sample

import (
	"testing"

	"github.com/foo/boah/models"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestAtomValidate(t *testing.T) {
	registry := strfmt.NewFormats()
	t.Run("ok", func(t *testing.T) {
		var name models.Name
		payload := []byte(`"foo"`)
		require.NoError(t, swag.ReadJSON(payload, &name))
		assert.NoError(t, name.Validate(registry))
	})
	t.Run("ng validate", func(t *testing.T) {
		var name models.Name
		payload := []byte(`""`)
		require.NoError(t, swag.ReadJSON(payload, &name))
		assert.Error(t, name.Validate(registry), "name is at least 1 chars long")
	})
	t.Run("ng load", func(t *testing.T) {
		var name models.Name
		payload := []byte(`100`)
		assert.Error(t, swag.ReadJSON(payload, &name), "age is must be int")
	})
}

func TestPrimitiveAtomValidate(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		var age int64
		payload := []byte(`100`)
		assert.NoError(t, swag.ReadJSON(payload, &age))
	})
	t.Run("ng load", func(t *testing.T) {
		var age int64
		payload := []byte(`"foo"`)
		assert.Error(t, swag.ReadJSON(payload, &age))
	})
}
