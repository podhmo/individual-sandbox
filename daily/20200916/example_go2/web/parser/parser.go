package parser

import (
	"encoding/json"
	"fmt"
	"io"
	"m/store/entity"
)

func New() *Parser {
	return &Parser{}
}

type Parser struct {
	// if needed, see config
}

func (p *Parser) Todo(r io.Reader, ob *entity.Todo) (err error) {
	decoder := json.NewDecoder(r)
	defer func() {
		if r, ok := r.(io.Closer); ok {
			closeErr := r.Close()
			if err != nil {
				err = closeErr
			}
		}
	}()

	err = decoder.Decode(ob)
	if err != nil {
		return fmt.Errorf("parse: %w", err)
	}
	return nil
}
