package storegolden

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"testing"
)

func filepathDefault(t *testing.T) string {
	return fmt.Sprintf("%s.golden", filepath.Join("testdata", t.Name()))
}

// Recorder :
type Recorder struct {
	Filepath func(*testing.T) string
	Exists   func(string) bool
	Encode   func(io.Writer, interface{}) error
	Decode   func(io.Reader, interface{}) error
}

// NewJSONRecorder :
func NewJSONRecorder() *Recorder {
	return &Recorder{
		Filepath: filepathDefault,
		Exists: func(fpath string) bool {
			_, err := os.Stat(fpath)
			return err == nil
		},
		Encode: func(w io.Writer, val interface{}) error {
			encoder := json.NewEncoder(w)
			encoder.SetIndent("", "  ")
			encoder.SetEscapeHTML(false)
			return encoder.Encode(val)
		},
		Decode: func(r io.Reader, val interface{}) error {
			decoder := json.NewDecoder(r)
			return decoder.Decode(val)
		},
	}
}

// Record :
func (r *Recorder) Record(t *testing.T, val interface{}) (err error) {
	fpath := r.Filepath(t)
	if err := os.Mkdir(filepath.Dir(fpath), 0744); err != nil {
		if !os.IsExist(err) {
			return err
		}
	}
	wf, err := os.Create(fpath)
	if err != nil {
		return err
	}
	defer func() {
		if cerr := wf.Close(); cerr != nil {
			err = cerr
		}
	}()
	return r.Encode(wf, val)
}

// Replay :
func (r *Recorder) Replay(t *testing.T, want interface{}) (err error) {
	rf, err := os.Open(r.Filepath(t))
	if err != nil {
		return err
	}
	defer func() {
		if cerr := rf.Close(); cerr != nil {
			err = cerr
		}
	}()
	if err := r.Decode(rf, want); err != nil {
		return err
	}
	return nil
}

// RecordAndReplay :
func (r *Recorder) RecordAndReplay(t *testing.T, got interface{}, assertion func(got, want interface{})) {
	fpath := r.Filepath(t)
	exists := r.Exists(fpath)
	if !exists {
		if err := r.Record(t, got); err != nil {
			t.Fatalf("record: %s", err)
		}
	}
	var want string
	if err := r.Replay(t, &want); err != nil {
		t.Fatalf("replay: %s", err)
	}
	if want != got {
		t.Fatalf("want=%q, but got=%q", want, got)
	}
	assertion(got, want)
}
