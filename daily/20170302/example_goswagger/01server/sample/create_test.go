package sample

import (
	"net/http"
	"testing"

	"github.com/foo/booah/models"
	"github.com/foo/booah/restapi/operations"
	"github.com/go-openapi/runtime/middleware"
	"github.com/go-openapi/strfmt"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestCreatePerson(t *testing.T) {
	route := &middleware.MatchedRoute{Formats: &registry}

	t.Run("ok", func(t *testing.T) {
		req, err := http.NewRequest("POST", "", nil)
		require.NoError(t, err)

		person := &models.Person{
			Name: models.Name("foo"),
			Age:  10,
		}
		params := operations.PostPersonParams{
			Body: person,
		}
		assert.NoError(t, params.BindRequest(req, route))
	})
	t.Run("ng", func(t *testing.T) {
		req, err := http.NewRequest("POST", "", nil)
		require.NoError(t, err)

		person := &models.Person{
			Name: models.Name(""),
			Age:  10,
		}
		params := operations.PostPersonParams{
			Body: person,
		}
		assert.NoError(t, params.BindRequest(req, route))
		registry := strfmt.NewFormats()
		require.NoError(t, person.Validate(registry))
	})
}
