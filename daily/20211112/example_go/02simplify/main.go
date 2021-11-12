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
// TODO: concurrent

type Entry interface {
	Name() string
	Hash() []byte
}

type Committer struct {
	Prev []Entry
}

func (c *Committer) Commit(entries []Entry) ([]Action, error) {
	prevEntries := c.Prev
	actions := make([]Action, len(entries))

	prevUsedCounter := make([]int, len(prevEntries))
	hasPrev := len(prevEntries) > 0

	for i, entry := range entries {
		if !hasPrev {
			actions[i] = Action{Type: ActionTypeCreate, entry: entry}
			continue
		}

		name := entry.Name()
		var prev Entry
		for j, x := range prevEntries {
			if x.Name() == name {
				prevUsedCounter[j]++
				prev = x
				break
			}
		}
		if prev == nil {
			actions[i] = Action{Type: ActionTypeCreate, entry: entry}
			continue
		}
		if bytes.Equal(prev.Hash(), entry.Hash()) {
			actions[i] = Action{Type: ActionTypeNotModified, entry: entry}
			continue
		}
		actions[i] = Action{Type: ActionTypeUpdate, entry: entry}
	}

	if hasPrev {
		for i, c := range prevUsedCounter {
			if c == 0 {
				entry := prevEntries[i]
				actions = append(actions, Action{Type: ActionTypeDelete, entry: entry})
			}
		}
	}
	return actions, nil
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
	entry Entry
}

func (a Action) String() string {
	return fmt.Sprintf("<Action type=%s, entry=%s>", a.Type, a.entry)
}
func (a Action) Name() string {
	return a.entry.Name()
}

type manager struct {
	hash    hash.Hash
	mu      sync.Mutex
	entries []Entry
}

func (m *manager) NewEntry(filename string, content []byte) Entry {
	entry := &lazyentry{
		name: filename,
		Size: -1,
		calc: func() ([]byte, int64) {
			m.mu.Lock()
			defer m.mu.Unlock()
			m.hash.Reset()
			size, err := io.Copy(m.hash, bytes.NewBuffer(content))
			if err != nil {
				log.Printf("ERROR: name=%s, %+v", filename, err)
				return []byte{}, 0
			}
			hash := m.hash.Sum(nil)
			return hash, size
		},
	}
	m.entries = append(m.entries, entry)
	return entry
}

type lazyentry struct {
	name string
	Size int64
	hash []byte // uninitialized=nil
	calc func() ([]byte, int64)
}

func (e *lazyentry) Name() string {
	return e.name
}

func (e *lazyentry) Hash() []byte {
	if e.hash != nil {
		return e.hash
	}
	e.hash, e.Size = e.calc()
	return e.hash
}

func (e *lazyentry) String() string {
	hash := e.Hash()
	return fmt.Sprintf("<Entry size=%d, name=%q, hash=%x>", e.Size, e.Name(), hash)
}
