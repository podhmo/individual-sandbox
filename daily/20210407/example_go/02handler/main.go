package main

import (
	"context"
	"fmt"
	"log"
	"net/http"
	"strings"

	"github.com/getkin/kin-openapi/openapi3"
	"github.com/getkin/kin-openapi/openapi3filter"
	"github.com/getkin/kin-openapi/routers/gorillamux"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	addr := ":8888"
	log.Println("listen ...", addr)

	loader := openapi3.NewSwaggerLoader()
	doc, err := loader.LoadSwaggerFromData([]byte(`
openapi: 3.1.0
info:
  version: 0.0.0
  title: my api
components:
  schemas:
    person:
      properties:
        name:
          type: string
        age:
          type: integer
      required:
        - name
      additionalProperties: false
paths:
  /:
    get:
      responses:
        "200":
          description: ok
          content:
            application/json:
              schema:
                type: object
    post:
      requestBody:
        required: true
        content:
          application/json:
            schema:
              $ref: "#/components/schemas/person"
      responses:
        "200":
          description: ok
          content:
            application/json:
              schema:
                type: object
`))
	if err != nil {
		return err
	}

	if err := doc.Validate(context.Background()); err != nil {
		return err
	}

	method := "POST"
	uri := "http://localhost:8888/"
	req, err := http.NewRequest(method, uri, strings.NewReader(`{"name": "foo"}`))
	req.Header.Set("Content-Type", "application/json")
	if err != nil {
		return err
	}

	router, err := gorillamux.NewRouter(doc)
	if err != nil {
		return err
	}
	route, pathParams, err := router.FindRoute(req)
	if err != nil {
		return err
	}
	input := &openapi3filter.RequestValidationInput{
		Request:     req,
		PathParams:  pathParams,
		QueryParams: nil, // TODO
		Route:       route,
		Options: &openapi3filter.Options{
			MultiError:            true,
			IncludeResponseStatus: true,
			AuthenticationFunc: func(context.Context, *openapi3filter.AuthenticationInput) error {
				return nil
			},
		},
	}

	fmt.Println("@", openapi3filter.ValidateRequest(context.Background(), input))
	return nil
}
