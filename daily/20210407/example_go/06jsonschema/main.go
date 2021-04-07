package main

import (
	"context"
	"encoding/json"
	"fmt"
	"log"

	"github.com/qri-io/jsonschema"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	ctx := context.Background()
	// $defsと言う名前で固定だった
	// https://json-schema.org/draft/2020-12/json-schema-core.html#defs
	var schemaData = []byte(`{
    "$id": "https://qri.io/schema/",
    "$comment" : "sample comment",
    "$ref": "#/$defs/P",
    "$defs": {
        "P": {
		  "type": "object",
		  "properties": {
			  "name": {
				  "type": "string"
			  },
			  "age": {
				  "description": "Age in years",
				  "type": "integer",
				  "minimum": 0
			  },
			  "friends": {
				"type" : "array",
				"items" : { "title" : "REFERENCE", "$ref" : "#/$defs/P" }
			  }
		  },
		  "required": ["name"]
        }
    }
  }`)

	rs := &jsonschema.Schema{}
	if err := json.Unmarshal(schemaData, rs); err != nil {
		return err
	}

	var valid = []byte(`{
    "name" : "George"
    }`)
	errs, err := rs.ValidateBytes(ctx, valid)
	if err != nil {
		return err
	}

	if len(errs) > 0 {
		return errs[0] // xxx
	}

	fmt.Println("----------------------------------------")
	var invalidPerson = []byte(`{}`)

	errs, err = rs.ValidateBytes(ctx, invalidPerson)
	if err != nil {
		return err
	}
	if len(errs) > 0 {
		fmt.Printf("%[1]T: %+[1]v\n", errs)
		return errs[0] // xxx
	}

	var invalidFriend = []byte(`{
    "name" : "Jay",
    "friends" : [{}]
    }`)
	errs, err = rs.ValidateBytes(ctx, invalidFriend)
	if err != nil {
		return err
	}
	if len(errs) > 0 {
		return errs[0] // xxx
	}
	return nil
}
