package foo

import (
	"bytes"
	"fmt"
)

type Entry interface {
	Name() string
	Hash() ([]byte, error)
}

type funcEntry struct {
	name     string
	hashFunc func() ([]byte, error)
}

func NewEntry(name string, hashFunc func() ([]byte, error)) Entry {
	return &funcEntry{
		name:     name,
		hashFunc: hashFunc,
	}
}
func (e *funcEntry) Name() string {
	return e.name
}
func (e *funcEntry) Hash() ([]byte, error) {
	return e.hashFunc()
}

// TODO: concurrent

func Classify(
	prevEntries, entries []Entry,
) ([]Action, error) {
	actions := make([]Action, len(entries))

	prevUsedCounter := make([]int, len(prevEntries))
	hasPrev := len(prevEntries) > 0

	for i, entry := range entries {
		if !hasPrev {
			actions[i] = Action{Type: ActionTypeCreate, Entry: entry}
			continue
		}

		var prev Entry
		name := entry.Name()
		for j, x := range prevEntries {
			if x.Name() == name {
				prevUsedCounter[j]++
				prev = x
				break
			}
		}

		if prev == nil {
			actions[i] = Action{Type: ActionTypeCreate, Entry: entry}
			continue
		}

		currentHash, err := entry.Hash()
		if err != nil {
			return nil, fmt.Errorf("get hash of current entry=%s: %w", name, err)
		}
		prevHash, err := prev.Hash()
		if err != nil {
			return nil, fmt.Errorf("get hash of previous entry=%s: %w", name, err)
		}
		if bytes.Equal(currentHash, prevHash) {
			actions[i] = Action{Type: ActionTypeNotModified, Entry: entry}
			continue
		}
		actions[i] = Action{Type: ActionTypeUpdate, Entry: entry}
	}

	if hasPrev {
		for i, c := range prevUsedCounter {
			if c == 0 {
				entry := prevEntries[i]
				actions = append(actions, Action{Type: ActionTypeDelete, Entry: entry})
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
	Entry Entry
}

func (a Action) Name() string {
	return a.Entry.Name()
}

func (a Action) String() string {
	return fmt.Sprintf("%s %s", a.Type, a.Entry.Name())
}
