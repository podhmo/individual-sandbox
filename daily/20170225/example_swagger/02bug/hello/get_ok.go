package hello

import (
	"github.com/foo/boo/models"
	"github.com/foo/boo/restapi/operations"
	middleware "github.com/go-openapi/runtime/middleware"
)

// PostOk :
func PostOk(params operations.PostOkParams) middleware.Responder {
	payload := params.Body
	return operations.NewPostOkOK().WithPayload(models.DateText(*payload))
}
