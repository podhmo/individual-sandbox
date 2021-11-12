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

type FileEntry struct {
	Name string
	Hash []byte
	Size int64
}

func Load(dir string, files ...string) (*Store, error) {
	hash := sha1.New()
	entries := make(map[string]*FileEntry, len(files))

	for _, name := range files {
		f, err := os.Open(filepath.Join(dir, name))
		if err != nil {
			return nil, err
		}
		hash.Reset()
		size, err := io.Copy(hash, f)
		if err != nil {
			return nil, fmt.Errorf("on file %s: %w", name, err)
		}
		entries[name] = &FileEntry{Name: name, Size: size, Hash: hash.Sum(nil)}
	}

	return &Store{
		Names:   files,
		entries: entries,
	}, nil
}

type Store struct {
	entries map[string]*FileEntry
	Names   []string
	mu      sync.RWMutex
}

func (s *Store) GetHash(name string) ([]byte, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()
	e, ok := s.entries[name]
	if !ok {
		return []byte{}, fmt.Errorf("not found")
	}
	return e.Hash, nil
}

func run() error {
	dir := DefinedDir(run)
	fmt.Println(DefinedDir(run))

	s, err := Load(filepath.Join(dir, "testdata"), "hello.txt", "byebye.txt", "hello2.txt")
	if err != nil {
		return fmt.Errorf("load entries: %w", err)
	}
	actions, err := foo.Classified(nil, s.Names, s.GetHash)
	if err != nil {
		return fmt.Errorf("classified: %w", err)
	}

	for _, ac := range actions {
		fmt.Println("-", ac.String())
	}
	return nil
}
