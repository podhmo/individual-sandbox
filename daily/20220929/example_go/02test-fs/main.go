package main

import (
	"fmt"
	"io/fs"
	"log"
	"testing/fstest"
)

func main() {
	if err := run(); err != nil {
		log.Printf("!! %+v", err)
	}
}

func run() error {
	root := fstest.MapFS{
		"foo/bar/x": &fstest.MapFile{Data: []byte("hello")},
		"foo/bar/y": &fstest.MapFile{},
		"foo/bar/z": &fstest.MapFile{},
		"foo/boo/i": &fstest.MapFile{},
		"foo/boo/j": &fstest.MapFile{},
		"main.go":   &fstest.MapFile{},
	}

	return fs.WalkDir(root, "foo", func(path string, d fs.DirEntry, err error) error {
		fmt.Println(path)
		// return fs.SkipDir
		return nil
	})
}
