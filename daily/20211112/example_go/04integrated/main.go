package main

import (
	"crypto/sha1"
	"fmt"
	"io"
	"log"
	"m/03integrated/foo"
	"os"
	"path/filepath"
	"reflect"
	"runtime"
	"sync"
)

func DefinedDir(fn interface{}) string {
	rfunc := runtime.FuncForPC(reflect.ValueOf(fn).Pointer())
	fname, _ := rfunc.FileLine(rfunc.Entry())
	return filepath.Dir(fname)
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type lazyFileEntry struct {
	Size int64

	name string
	hash []byte
	err  error

	hashFunc func(string) ([]byte, int64, error)
}

func (e *lazyFileEntry) Name() string {
	return e.name
}
func (e *lazyFileEntry) String() string {
	hash, _ := e.Hash()
	return fmt.Sprintf("%s %s", e.name, hash)
}
func (e *lazyFileEntry) Hash() ([]byte, error) {
	if e.hash != nil {
		return e.hash, e.err
	}
	e.hash, e.Size, e.err = e.hashFunc(e.name)
	return e.hash, e.err
}

func Load(dir string, files ...string) ([]foo.Entry, error) {
	hash := sha1.New()
	entries := make([]foo.Entry, len(files))

	var mu sync.Mutex
	for i, name := range files {
		entries[i] = &lazyFileEntry{
			name: name,
			hashFunc: func(name string) ([]byte, int64, error) {
				f, err := os.Open(filepath.Join(dir, name))
				if err != nil {
					return nil, 0, fmt.Errorf("on file %s: %w", name, err)
				}
				mu.Lock()
				defer mu.Unlock()
				hash.Reset()
				size, err := io.Copy(hash, f)
				if err != nil {
					return nil, 0, fmt.Errorf("calc hash on file %s: %w", name, err)
				}
				return hash.Sum(nil), size, nil
			},
		}
	}
	return entries, nil
}

func run() error {
	dir := DefinedDir(run)
	fmt.Println(DefinedDir(run))

	prev, err := Load(filepath.Join(dir, "testdata/src"), "hello.txt", "byebye.txt", "hello2.txt")
	if err != nil {
		return fmt.Errorf("load entries: %w", err)
	}
	entries, err := Load(filepath.Join(dir, "testdata/change1"), "hello.txt", "hello3.txt")
	if err != nil {
		return fmt.Errorf("load entries: %w", err)
	}
	actions, err := foo.Classify(prev, entries)
	if err != nil {
		return fmt.Errorf("classified: %w", err)
	}

	for _, ac := range actions {
		fmt.Println("-", ac.String())
	}
	return nil
}
