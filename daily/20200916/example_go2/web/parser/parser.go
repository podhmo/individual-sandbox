package parser

import (
	"encoding/json"
	"io"
	"m/store/entity"
	"strconv"

	"golang.org/x/xerrors"
)

func New() *Parser {
	return &Parser{}
}

type Parser struct {
	// if needed, see config
}

func (p *Parser) Int64(s string, n *int64) error {
	v, err := strconv.ParseInt(s, 10, 64)
	if err != nil {
		// TODO: 400 error
		return xerrors.Errorf("parse: %w", err)
	}
	*n = v
	return nil
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
		return xerrors.Errorf("parse: %w", err)
	}
	return nil
}
