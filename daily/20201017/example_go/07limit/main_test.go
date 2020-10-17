package main

import (
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"reflect"
	"strconv"
	"strings"
	"testing"

	"github.com/podhmo/tenuki"
)

type Todo struct {
	Title string `json:"title"`
	Done  bool   `json:"done"`
}

func ListTodo(w http.ResponseWriter, req *http.Request) {
	q := req.URL.Query()
	limit, err := strconv.ParseInt(q.Get("limit"), 10, 0)
	if err != nil {
		tenuki.Render(w, req).JSON(400, map[string]string{"message": err.Error()})
	}
	items := []Todo{
		{Title: "hello."},
		{Title: "byebye."},
	}
	if limit > 0 && len(items) > int(limit) {
		items = items[:limit]
	}
	tenuki.Render(w, req).JSONArray(200, items)
}

type Client struct {
	BaseURL string
	*Driver
	*Decoder
}

type Driver struct {
	HTTPClient *http.Client
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

type ListInput struct {
	Limit int
}

func NewListInput() ListInput {
	return ListInput{Limit: 10}
}

type withLimitFunc func(*ListInput)

func WithLimit(limit int) withLimitFunc {
	return func(input *ListInput) {
		input.Limit = limit
	}
}

type ListTodoInput struct {
	ListInput
}
type listTodoInputOption interface {
	ApplyListTodoInput(*ListTodoInput)
}

func (f withLimitFunc) ApplyListTodoInput(input *ListTodoInput) {
	f(&input.ListInput)
}
func (input *ListTodoInput) RequestInput(body io.Reader) RequestInput {
	rinput := NewRequestInput(body)
	rinput.Query["limit"] = strconv.Itoa(input.Limit)
	return rinput
}

func (c *Client) ListTodo(options ...listTodoInputOption) ([]Todo, error) {
	input := &ListTodoInput{
		ListInput: NewListInput(),
	}
	for _, opt := range options {
		opt.ApplyListTodoInput(input)
	}

	url := c.BaseURL + "/todo"
	req, err := c.Driver.NewRequest("GET", url, input.RequestInput(nil))
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
		got, err := client.ListTodo(WithLimit(1))
		if err != nil {
			t.Errorf("unexpected error: %+v", err)
		}

		want := []Todo{Todo{Title: "hello"}}
		if !reflect.DeepEqual(want, got) {
			t.Errorf("response body\nwant\n\t%+v\nbut\n\t%+v", want, got)
		}
	})
}
