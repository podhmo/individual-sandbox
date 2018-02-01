package main

import (
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
)

func main() {
	os.Mkdir("foo", 0755)
	os.MkdirAll("foo2/bar/boo", 0755)
	if err := ioutil.WriteFile(filepath.Join("foo2/bar/boo", "person.go"), []byte("hai"), 0744); err != nil {
		log.Fatal(err)
	}
}
