package main

import (
	"encoding/json"

	"github.com/k0kubun/pp"
	"reflect"
)

var b = []byte(`
{
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "example": "Homebridge"
        },
        "username": {
          "type": "string",
          "example": "CC:22:3D:E3:CE:30"
        },
        "port": {
          "type": "integer",
          "example": 51826
        },
        "pin": {
          "type": "string",
          "example": "031-45-154"
        }
      },
      "required": [
        "name",
        "username",
        "port",
        "pin"
      ]
    }
`)

func build(ob interface{}, r []string) error {
    //
}

func main() {
	var ob interface{}
	json.Unmarshal(b, &ob)
	pp.Print(ob)
}
