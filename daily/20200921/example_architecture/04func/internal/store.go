package internal

import (
	"encoding/json"
	"fmt"
	"os"
)

type Loader struct {
}

func (l *Loader) Load(filename string, store *[]Todo) error {
	f, err := os.Open(filename)
	if err != nil {
		return fmt.Errorf("load open: %w", err)
	}
	defer f.Close() // todo defer

	decoder := json.NewDecoder(f)
	if err := decoder.Decode(store); err != nil {
		return fmt.Errorf("load decode: %w", err)
	}
	return nil
}

func (l *Loader) Save(filename string, store []Todo) error {
	f, err := os.Create(filename)
	if err != nil {
		return fmt.Errorf("save create: %w", err)
	}
	defer f.Close()

	encoder := json.NewEncoder(f)
	encoder.SetIndent("", "  ")
	if err := encoder.Encode(&store); err != nil {
		return fmt.Errorf("save encode: %w", err)
	}
	return nil
}
