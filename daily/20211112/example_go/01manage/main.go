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

	current history
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
	m.current.entries = append(m.current.entries, entry)
	return entry
}

func (m *manager) Commit() ([]Action, error) {
	return m.CommitWith(nil)
}

// TODO: concurrent

func (m *manager) CommitWith(prevHistory *history) ([]Action, error) {
	entries := m.current.entries
	actions := make([]Action, len(entries))
	usedCounter := make([]int, len(entries))

	for i, entry := range entries {
		usedCounter[i]++
		if prevHistory == nil {
			actions[i] = Action{Type: ActionTypeCreate, entry: entry}
			continue
		}

		prev, existed := prevHistory.Get(entry.Name)
		if !existed {
			actions[i] = Action{Type: ActionTypeCreate, entry: entry}
			continue
		}
		if bytes.Equal(prev.Hash, entry.Hash) {
			actions[i] = Action{Type: ActionTypeNotModified, entry: entry}
			continue
		}
		actions[i] = Action{Type: ActionTypeUpdate, entry: entry}
	}

	if prevHistory != nil {
		for i, c := range prevHistory.usedCount {
			if c == 0 {
				entry := prevHistory.entries[i]
				actions = append(actions, Action{Type: ActionTypeDelete, entry: entry})
			}
		}
	}
	return actions, nil
}

type history struct {
	entries   []entry
	usedCount []int
}

func (h *history) Get(name string) (entry, bool) {
	for i, e := range h.entries {
		if e.Name == name {
			h.usedCount[i]++
			return e, true
		}
	}
	return entry{}, false
}

type ActionType string

const (
	ActionTypeUNKNOWN     ActionType = ""
	ActionTypeCreate      ActionType = "C"
	ActionTypeUpdate      ActionType = "U"
	ActionTypeDelete      ActionType = "D"
	ActionTypeNotModified ActionType = "-"
)

type Action struct {
	Type  ActionType
	entry entry
}

func (a Action) String() string {
	return fmt.Sprintf("<Action type=%s, entry=%s>", a.Type, a.entry)
}
func (a Action) Name() string {
	return a.entry.Name
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
