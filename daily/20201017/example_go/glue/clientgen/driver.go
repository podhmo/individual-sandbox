package clientgen

import (
	"io"
	"io/ioutil"
	"net/http"
)

func NewDriver() *Driver {
	return &Driver{HTTPClient: http.DefaultClient}
}

type Driver struct {
	HTTPClient *http.Client
}

func (d *Driver) NewRequest(method, url string, input RequestInput) (*http.Request, error) {
	// todo: authorization?
	req, err := http.NewRequest(method, url, input.Body)
	if err != nil {
		return nil, err
	}

	if len(input.Header) > 0 {
		for k, v := range input.Header {
			req.Header.Add(k, v)
		}
	}
	if len(input.Query) > 0 {
		url := req.URL
		q := url.Query()
		for k, v := range input.Query {
			q.Set(k, v)
		}
		req.URL.RawQuery = q.Encode()
	}
	return req, nil
}

func (d *Driver) Do(req *http.Request) (*http.Response, error, func()) {
	resp, err := d.HTTPClient.Do(req)
	return resp, err, func() {
		io.Copy(ioutil.Discard, resp.Body)
		resp.Body.Close()
	}
}

type RequestInput struct {
	Header map[string]string
	Query  map[string]string
	Body   io.Reader
}

func NewRequestInput(body io.Reader) RequestInput {
	return RequestInput{
		Body:   body,
		Query:  map[string]string{},
		Header: map[string]string{},
	}
}
