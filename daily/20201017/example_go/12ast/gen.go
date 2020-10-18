package main

type (
	__ACTION__ string
	__METHOD__ string
	__PATH__   string
)

type __RETURN_TYPE__ string

type __ACTION__Input struct {
	// TODO:
}

func New__ACTION__Input() *__ACTION__Input {
	return &__ACTION__Input{}
}

type __ACTION__InputOption interface {
	Apply__ACTION__(*__ACTION__Input)
}

func (c *Client) __ACTION__(options ...__ACTION__InputOption) (__RETURN_TYPE__, error) {
	input := New__ACTION__Input()
	for _, opt := range options {
		opt.Apply__ACTION__Input(input)
	}

	url := c.BaseURL + __PATH__
	req, err := c.Driver.NewRequest(__METHOD__, url, input.RequestInput(nil))
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

	var items __RETURN_TYPE__
	if c.Decoder.DecodeResult(resp, &items); err != nil {
		return nil, err
	}
	return items, nil
}
