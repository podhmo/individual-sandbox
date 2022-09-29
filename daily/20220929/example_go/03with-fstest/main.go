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
	root := FS(
		Dir("foo",
			Dir("bar", File("x", nil), File("y", nil), File("z", nil)),
			Dir("boo", File("i", nil), File("j", nil))),
	)
	return fs.WalkDir(root, "foo", func(path string, d fs.DirEntry, err error) error {
		fmt.Println("@", path)
		return nil
	})
}

func FS(d *dir) fstest.MapFS {
	fs := fstest.MapFS{}
	d.Walk(fs, "")
	return fs
}
func Dir(name string, files ...Node) *dir {
	return &dir{name: name, children: files}
}
func File(name string, body []byte) *file {
	return &file{name: name, body: body}
}

type Node interface {
	Walk(fs fstest.MapFS, prefix string)
}

type dir struct {
	name     string
	children []Node
}

func (d *dir) Walk(fs fstest.MapFS, prefix string) {
	name := d.name
	if prefix != "" {
		name = prefix + "/" + d.name
	}
	for _, child := range d.children {
		child.Walk(fs, name)
	}
}

type file struct {
	name string
	body []byte
}

func (f *file) Walk(fs fstest.MapFS, prefix string) {
	name := f.name
	if prefix != "" {
		name = prefix + "/" + name
	}
	fs[name] = &fstest.MapFile{Data: f.body}
}
