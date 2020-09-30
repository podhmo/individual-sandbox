package main

import (
	"github.com/getkin/kin-openapi/openapi3"
	"github.com/k0kubun/pp"
)

const spec = `
{
  "info": {
    "description": "A sample API to illustrate OpenAPI concepts",
    "title": "Sample API",
    "version": "1.0.0"
  },
  "openapi": "3.0.0",
  "paths": {
    "/path1": {
      "get": {
        "responses": {
          "200": {
            "description": ""
          }
        }
      }
    }
  }
}
`

func main() {
	spec := []byte(spec)
	loader := openapi3.NewSwaggerLoader()
	doc, err := loader.LoadSwaggerFromData(spec)

	pp.Println(doc, err)
}
