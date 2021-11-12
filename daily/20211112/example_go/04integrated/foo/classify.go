package foo

import (
	"bytes"
	"fmt"
)

// TODO: concurrent

func Classified(
	prevEntries, entries []string,
	hashFunc func(string) ([]byte, error),
) ([]Action, error) {
	actions := make([]Action, len(entries))

	prevUsedCounter := make([]int, len(prevEntries))
	hasPrev := len(prevEntries) > 0

	for i, name := range entries {
		if !hasPrev {
			actions[i] = Action{ActionType: ActionTypeCreate, Name: name}
			continue
		}

		var prev *string
		for j, x := range prevEntries {
			if x == name {
				prevUsedCounter[j]++
				prev = &x
				break
			}
		}

		if prev == nil {
			actions[i] = Action{ActionType: ActionTypeCreate, Name: name}
			continue
		}

		currentHash, err := hashFunc(name)
		if err != nil {
			return nil, fmt.Errorf("get hash of current entry=%s: %w", name, err)
		}
		prevHash, err := hashFunc(*prev)
		if err != nil {
			return nil, fmt.Errorf("get hash of previous entry=%s: %w", name, err)
		}
		if bytes.Equal(currentHash, prevHash) {
			actions[i] = Action{ActionType: ActionTypeNotModified, Name: name}
			continue
		}
		actions[i] = Action{ActionType: ActionTypeUpdate, Name: name}
	}

	if hasPrev {
		for i, c := range prevUsedCounter {
			if c == 0 {
				name := prevEntries[i]
				actions = append(actions, Action{ActionType: ActionTypeDelete, Name: name})
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
	ActionType ActionType
	Name       string
}

func (a Action) String() string {
	return fmt.Sprintf("%s %s", a.ActionType, a.Name)
}
