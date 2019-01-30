package main

import (
	"fmt"
	"log"

	"github.com/evanphx/json-patch"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	original := []byte(`{"name": "foo", "age": 20, "options": {"nickname": "f"}}`)
	target := []byte(`{"name": "foo","age": 21, "options": {"x": "y"}}`)

	patch, err := jsonpatch.CreateMergePatch(original, target)
	if err != nil {
		return err
	}
	fmt.Println("patch", string(patch))

	return nil
}
