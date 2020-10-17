package main

import (
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"reflect"
	"strings"
	"testing"

	"github.com/podhmo/tenuki"
)

type Todo struct {
	Title string `json:"title"`
	Done  bool   `json:"done"`
}

func ListTodo(w http.ResponseWriter, req *http.Request) {
	tenuki.Render(w, req).JSONArray(201, []Todo{{
		Title: "hello",
	}})
}

type Client struct {
	BaseURL string
	*Driver
	*Decoder
}

type Driver struct {
	HTTPClient *http.Client
}

func (d *Driver) NewRequest(method, url string, body io.Reader) (*http.Request, error) {
	// todo: authorization?
	return http.NewRequest(method, url, body)
}
func (d *Driver) Do(req *http.Request) (*http.Response, error, func()) {
	resp, err := d.HTTPClient.Do(req)
	return resp, err, func() {
		io.Copy(ioutil.Discard, resp.Body)
		resp.Body.Close()
	}
}

type Decoder struct {
}

func (t *Decoder) DecodeError(resp *http.Response) error {
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("error: %s", resp.Status)
	}
	return nil
}
func (t *Decoder) DecodeResult(resp *http.Response, result interface{}) error {
	decoder := json.NewDecoder(resp.Body)
	if err := decoder.Decode(result); err != nil {
		return fmt.Errorf("decode json: %w", err)
	}
	return nil
}

func NewClient(baseURL string, options ...func(*Client)) *Client {
	client := &Client{
		BaseURL: strings.TrimSuffix(baseURL, "/"),
		Driver: &Driver{
			HTTPClient: http.DefaultClient,
		},
		Decoder: &Decoder{},
	}
	for _, opt := range options {
		opt(client)
	}
	return client
}
func WithHTTPClient(httpClient *http.Client) func(*Client) {
	return func(c *Client) {
		c.HTTPClient = httpClient
	}
}

func (c *Client) ListTodo() ([]Todo, error) {
	req, err := c.Driver.NewRequest(
		"GET",
		c.BaseURL+"/todo",
		nil,
	)
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

	var items []Todo
	if c.Decoder.DecodeResult(resp, &items); err != nil {
		return nil, err
	}
	return items, nil
}

func TestIt(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(ListTodo))
	defer ts.Close()

	ct := &tenuki.CapturedTransport{}
	client := NewClient(
		ts.URL,
		WithHTTPClient(&http.Client{Transport: ct}),
	)

	t.Run("list", func(t *testing.T) {
		defer ct.Capture(t)()
		got, err := client.ListTodo()
		if err != nil {
			t.Errorf("unexpected error: %+v", err)
		}

		want := []Todo{Todo{Title: "hello"}}
		if !reflect.DeepEqual(want, got) {
			t.Errorf("response body\nwant\n\t%+v\nbut\n\t%+v", want, got)
		}
	})
}
