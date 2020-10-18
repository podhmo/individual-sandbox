package clientgen

import (
	"io"
)

type __Action__ string
type __ReturnType__ *string

var (
	__Path__   string = ""
	__Method__ string = ""
)

type __Action__Input struct {
}

func (input *__Action__Input) RequestInput(body io.Reader) RequestInput {
	return NewRequestInput(body)
}
func New__Action__Input() *__Action__Input {
	return &__Action__Input{}
}

type __Action__InputOption interface {
	Apply__Action__(*__Action__Input)
}

func (c *Client) __Action__(options ...__Action__InputOption) (__ReturnType__, error) {
	input := New__Action__Input()
	for _, opt := range options {
		opt.Apply__Action__(input)
	}

	url := c.BaseURL + __Path__
	req, err := c.Driver.NewRequest(__Method__, url, input.RequestInput(nil))
	if err != nil {
		return nil, err
	}

	resp, err, cleanup := c.Driver.Do(req)
	if err != nil {
		return nil, err
	}
	defer cleanup()

	if err := c.Decoder.DecodeError(resp); err != nil {
		return nil, err
	}

	var items __ReturnType__
	if c.Decoder.DecodeResult(resp, &items); err != nil {
		return nil, err
	}
	return items, nil
}
