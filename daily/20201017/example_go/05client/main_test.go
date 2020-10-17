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
	HTTPClient *http.Client
	BaseURL    string
}

func NewClient(baseURL string, options ...func(*Client)) *Client {
	client := &Client{
		BaseURL:    strings.TrimSuffix(baseURL, "/"),
		HTTPClient: http.DefaultClient,
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
	method := "GET"
	url := c.BaseURL + "/todo"
	var body io.Reader

	req, err := http.NewRequest(method, url, body)
	if err != nil {
		return nil, err
	}
	// todo: authorization?

	resp, err := c.HTTPClient.Do(req)
	if err != nil {
		return nil, err
	}
	defer func() {
		io.Copy(ioutil.Discard, resp.Body)
		resp.Body.Close()
	}()

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("error: %s", resp.Status)
	}

	var items []Todo
	decoder := json.NewDecoder(resp.Body)
	if decoder.Decode(&items); err != nil {
		return nil, fmt.Errorf("decode json: %w", err)
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
