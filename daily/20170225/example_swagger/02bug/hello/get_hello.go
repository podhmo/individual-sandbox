package hello

import (
	"github.com/foo/boo/restapi/operations"
	middleware "github.com/go-openapi/runtime/middleware"
)

// PostHello :
func PostHello(params operations.PostHelloParams) middleware.Responder {
	payload := params.Body
	return operations.NewPostHelloOK().WithPayload(payload)
}
