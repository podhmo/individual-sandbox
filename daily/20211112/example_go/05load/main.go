package main

import (
	"crypto/sha1"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"m/03integrated/foo"
	"os"
	"path/filepath"
	"reflect"
	"runtime"
	"strconv"
	"sync"
	"time"
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

type fileinfo struct {
	Name  string    `json:"name"`
	Hash  []byte    `json:"hash"`
	Mtime time.Time `json:"mtime"`
}

type FileEntry struct {
	fileinfo
}

func (e *FileEntry) Name() string {
	return e.fileinfo.Name
}
func (e *FileEntry) String() string {
	return fmt.Sprintf("%s %s", e.fileinfo.Name, e.fileinfo.Hash)
}
func (e *FileEntry) Hash() ([]byte, error) {
	return e.fileinfo.Hash, nil
}
func (e *FileEntry) Mtime() time.Time {
	return e.fileinfo.Mtime
}

func LoadFromJSONFile(filename string) ([]foo.Entry, error) {
	f, err := os.Open(filename)
	if err != nil {
		if os.IsNotExist(err) {
			return []foo.Entry{}, nil
		}
		return nil, fmt.Errorf("open %s: %w", filename, err)
	}
	defer f.Close()
	decoder := json.NewDecoder(f)
	var entries []*FileEntry
	if err := decoder.Decode(&entries); err != nil {
		return nil, fmt.Errorf("decode json %s: %w", filename, err)
	}

	dst := make([]foo.Entry, len(entries))
	for i, x := range entries {
		dst[i] = x
	}
	return dst, nil
}

func SaveToJSONFile(filename string, src []foo.Action, mtime time.Time) error {
	dst := make([]fileinfo, 0, len(src))
	for _, ac := range src {
		if ac.Type == foo.ActionTypeDelete {
			continue
		}

		hash, err := ac.Entry.Hash()
		if err != nil {
			return err
		}
		mtime := mtime
		if t, ok := ac.Entry.(interface{ Mtime() time.Time }); ok {
			mtime = t.Mtime()
		}
		dst = append(dst, fileinfo{
			Name:  ac.Name(),
			Hash:  hash,
			Mtime: mtime,
		})
	}
	f, err := os.Create(filename)
	if err != nil {
		return fmt.Errorf("open file %s: %w", filename, err)
	}
	defer f.Close()

	encoder := json.NewEncoder(f)
	encoder.SetIndent("", "  ")
	encoder.SetEscapeHTML(false)
	if err := encoder.Encode(dst); err != nil {
		return fmt.Errorf("encode json %s: %w", filename, err)
	}
	return nil
}

func Load(files ...string) ([]foo.Entry, error) {
	hash := sha1.New()
	entries := make([]foo.Entry, len(files))

	var mu sync.Mutex
	for i, name := range files {
		dir := filepath.Dir(name)
		entries[i] = &lazyFileEntry{
			name: filepath.Base(name),
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

	var prev, current []foo.Entry
	if isClean, _ := strconv.ParseBool(os.Getenv("CLEAN")); isClean {
		files, err := filepath.Glob(filepath.Join(dir, "testdata/src/*.txt"))
		if err != nil {
			return fmt.Errorf("load entries (glob): %w", err)
		}
		entries, err := Load(files...)
		if err != nil {
			return fmt.Errorf("load entries: %w", err)
		}
		current = entries
	} else {
		{
			entries, err := LoadFromJSONFile("testdata/actions.json")
			if err != nil {
				return fmt.Errorf("load previous entries: %w", err)
			}
			prev = entries
		}
		{
			files, err := filepath.Glob(filepath.Join(dir, "testdata/change1/*.txt"))
			if err != nil {
				return fmt.Errorf("load entries (glob): %w", err)
			}
			entries, err := Load(files...)
			if err != nil {
				return fmt.Errorf("load entries: %w", err)
			}
			current = entries
		}

	}

	actions, err := foo.Classify(prev, current)
	if err != nil {
		return fmt.Errorf("classified: %w", err)
	}

	for _, ac := range actions {
		fmt.Println("-", ac.String())
	}
	if dryrun, _ := strconv.ParseBool(os.Getenv("DRYRUN")); !dryrun {
		if err := SaveToJSONFile("testdata/actions.json", actions, time.Now()); err != nil {
			return fmt.Errorf("save entries: %w", err)
		}
	}
	return nil
}
