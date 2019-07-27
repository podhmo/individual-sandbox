package replay

import (
	"encoding/json"
	"io"
	"os"
	"path/filepath"
)

// Loader :
type Loader struct {
	Encode func(io.Writer, interface{}) error
	Decode func(io.Reader, interface{}) error
}

// Save :
func (r *Loader) Save(fpath string, val interface{}) (err error) {
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

func (r *Loader) Load(fpath string, want interface{}) (err error) {
	rf, err := os.Open(fpath)
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

// NewJSONLoader :
func NewJSONLoader() *Loader {
	return &Loader{
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
