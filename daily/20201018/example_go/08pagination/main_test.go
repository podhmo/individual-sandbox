package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"strconv"
	"testing"
	"time"

	"github.com/google/go-querystring/query"
	"github.com/podhmo/tenuki"
)

func Handler(w http.ResponseWriter, r *http.Request) {
	time.Sleep(500 * time.Millisecond)

	q := r.URL.Query()
	var input ListInput
	var err error
	if v := q.Get("size"); v != "" {
		input.Size, err = strconv.Atoi(v)
		if err != nil {
			tenuki.Render(w, r).JSON(400, err.Error())
			return
		}
	}
	if v := q.Get("cursor"); v != "" {
		input.Cursor, err = strconv.Atoi(v)
		if err != nil {
			tenuki.Render(w, r).JSON(400, err.Error())
			return
		}
	}
	xs := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11}
	N := len(xs)
	if input.Cursor > 0 {
		if input.Cursor > len(xs) {
			xs = []int{}
		} else {
			xs = xs[input.Cursor:]
		}
	}
	if input.Size > 0 {
		if input.Size <= len(xs) {
			xs = xs[:input.Size]
		}
	}
	// time.Sleep(500 * time.Millisecond)
	q = r.URL.Query()
	q.Set("cursor", strconv.Itoa(input.Cursor+len(xs)))
	nextURL := r.URL.Host + r.URL.Path + "?" + q.Encode()
	tenuki.Render(w, r).JSON(200, ListOutput{
		Items:   xs,
		Next:    nextURL,
		HasNext: input.Cursor+len(xs) < N,
	})
}

type ListOutput struct {
	Items   []int  `json:"items"`
	Next    string `json:"next"`
	HasNext bool   `json:"hasNext"`
}

type Client struct {
	BaseURL    string
	HTTPClient *http.Client
}

type ListInput struct {
	Size   int `url:"size"`
	Cursor int `url:"cursor"`
}

type ClientListInput struct {
	ListInput
	ScanAll bool
}

func (c *Client) List(ctx context.Context, input ClientListInput) ([]ListOutput, error) {
	v, err := query.Values(input.ListInput)
	if err != nil {
		return nil, err
	}
	url := c.BaseURL + "/" + "?" + v.Encode()

	// TODO: log
	// TODO: bodyがある場合はここでcopy
	var r []ListOutput
	for {
		req, err := http.NewRequestWithContext(ctx, "GET", url, nil)
		if err != nil {
			return nil, err
		}
		resp, err := c.HTTPClient.Do(req)
		if err != nil {
			return nil, err
		}
		defer func() {
			io.Copy(ioutil.Discard, resp.Body)
			resp.Body.Close()
		}()
		if resp.StatusCode != 200 {
			return nil, fmt.Errorf("unexpected status: %v", resp.StatusCode)
		}
		var output ListOutput
		if err := json.NewDecoder(resp.Body).Decode(&output); err != nil {
			return nil, err
		}
		r = append(r, output)
		if !input.ScanAll || !output.HasNext {
			break
		}

		url = c.BaseURL + output.Next // ???
	}
	return r, nil
}

func TestIt(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(Handler))

	tc := &tenuki.CapturedTransport{T: t}
	tc.Transport = http.DefaultTransport
	c := &Client{
		BaseURL:    ts.URL,
		HTTPClient: &http.Client{Transport: tc},
	}
	t.Run("1", func(t *testing.T) {
		defer tc.Capture(t)()
		ctx, cancel := context.WithTimeout(context.Background(), 1*time.Second)
		defer cancel()
		got, err := c.List(ctx, ClientListInput{
			ListInput: ListInput{Size: 3, Cursor: 1},
			ScanAll:   true,
		})
		if err != nil {
			t.Errorf("!! %+v", err)
		}
		fmt.Println(got)
	})
}
