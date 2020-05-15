package load

import (
	"encoding/json"
	"io"

	"github.com/k0kubun/pp"
)

func Load(r io.Reader, ob interface{}) error {
	decoder := json.NewDecoder(r)
	return decoder.Decode(ob)
}

func LoadAndPrint(r io.Reader, ob interface{}) error {
	if err := Load(r, ob); err != nil {
		return err
	}
	pp.Println(ob)
	return nil
}
