package main

import (
	"fmt"
	"log"
	"reflect"

	_ "./statik" // まじめにやるならabsolute path
	"github.com/rakyll/statik/fs"
)

func main() {
	FS, err := fs.New()
	if err != nil {
		log.Fatal(err)
	}

	rm := reflect.ValueOf(FS).Elem().FieldByName("files")
	for _, k := range rm.MapKeys() {
		fmt.Println(k, string(rm.MapIndex(k).FieldByName("data").Bytes()))
	}
}
