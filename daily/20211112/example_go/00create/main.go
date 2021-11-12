package main

import (
	"bytes"
	"crypto/sha1"
	"fmt"
	"hash"
	"io"
	"log"
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

func run() error {
	fmt.Println(DefinedDir(run))
	m := &manager{hash: sha1.New()}
	{
		fmt.Println(m.NewEntry("hello.txt", []byte("hello")))
		fmt.Println(m.NewEntry("byebye.txt", []byte("byebye")))
		fmt.Println(m.NewEntry("hello2.txt", []byte("hello hello")))
	}
	fmt.Println("----------------------------------------")
	{
		fmt.Println(m.NewEntry("byebye.txt", []byte("byebye")))
		fmt.Println(m.NewEntry("hello.txt", []byte("hello")))
		fmt.Println(m.NewEntry("hello2.txt", []byte("hello hello")))
	}
	return nil
}

// TODO: created at
// TODO: lazy hash
// TODO: concurrent

type manager struct {
	hash hash.Hash
	mu   sync.Mutex
}

func (m *manager) NewEntry(filename string, content []byte) entry {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.hash.Reset()
	size, err := io.Copy(m.hash, bytes.NewBuffer(content))
	if err != nil {
		panic(err)
	}
	hash := m.hash.Sum(nil)
	entry := entry{
		Name: filename,
		Size: size,
		Hash: hash,
	}
	return entry
}

type entry struct {
	Name string
	Size int64
	Hash []byte
	// CreatedAt
}

func (e entry) String() string {
	return fmt.Sprintf("<Entry size=%d, name=%q, hash=%x>", e.Size, e.Name, e.Hash)
}
