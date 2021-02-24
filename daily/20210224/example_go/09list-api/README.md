```console
$ go run main.go
{
  "components": {
    "schemas": {
      "Info": {
        "properties": {
          "hasNext": {
            "type": "boolean"
          },
          "nextId": {
            "type": "string"
          }
        },
        "required": [
          "hasNext"
        ],
        "type": "object"
      },
      "User": {
        "properties": {
          "name": {
            "type": "string"
          }
        },
        "type": "object"
      }
    }
  },
  "info": {
    "description": "-",
    "title": "Sample API",
    "version": "0.0.0"
  },
  "openapi": "3.0.0",
  "paths": {
    "/users": {
      "get": {
        "operationId": "main.ListUserWithPagination",
        "parameters": [
          {
            "in": "query",
            "name": "page",
            "schema": {
              "type": "integer"
            }
          }
        ],
        "responses": {
          "200": {
            "content": {
              "application/json": {
                "schema": {
                  "properties": {
                    "info": {
                      "$ref": "#/components/schemas/Info"
                    },
                    "items": {
                      "items": {
                        "$ref": "#/components/schemas/User"
                      },
                      "type": "array"
                    }
                  },
                  "type": "object"
                }
              }
            },
            "description": ""
          },
          "default": {
            "description": ""
          }
        }
      }
    }
  },
  "servers": [
    {
      "url": "http://localhost:8888",
      "description": "local development server"
    }
  ]
}

----------------------------------------
API response: 200 OK /?page=0
{"items":[{"name":"foo0"},{"name":"foo1"},{"name":"foo2"}],"info":{"hasNext":true,"nextId":"1"}}
API response: 200 OK /?page=1
{"items":[{"name":"foo3"},{"name":"foo4"},{"name":"foo5"}],"info":{"hasNext":false}}
API response: 200 OK /?page=2
{"items":[],"info":{"hasNext":false}}
```
