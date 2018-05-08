package main

import (
	"io"
	"log"
	"net/http"
	"os"
	"path/filepath"
	"strings"

	_ "./statik"
	"github.com/rakyll/statik/fs"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

type DataLoader struct {
	prefix string
	fs     http.FileSystem
}

func (o *DataLoader) Open(path string) (http.File, error) {
	return o.fs.Open(filepath.Join(o.prefix, path))
}

func New(prefix string) (*DataLoader, error) {
	FS, err := fs.New()
	if err != nil {
		return nil, err
	}
	if !strings.HasPrefix(prefix, "/") {
		prefix = "/" + prefix
	}
	return &DataLoader{
		fs:     FS,
		prefix: prefix,
	}, nil
}

func run() error {
	o, err := New("dev") // or "production"
	if err != nil {
		return err
	}

	f, err := o.Open("config.json")
	if err != nil {
		return err
	}
	defer f.Close()
	_, err = io.Copy(os.Stdout, f)
	if err != nil {
		return err
	}
	return nil
}
