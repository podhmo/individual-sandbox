package main

import (
	"context"
	"encoding/json"
	"io"
	"net/http"
	"path"
	"time"
)

type APIError struct {
	StatusCode int
	Message    string
	Response   *http.Response
}

func (e *APIError) Error() string {
	return e.Message
}

type Client struct {
	BasePath          string
	HTTPClient        *http.Client
	MaxNetworkRetries int
}

func (c *Client) newRequest(ctx context.Context, method, url string, body io.Reader) (*http.Request, error) {
	// Add Authorization header
	req, err := http.NewRequestWithContext(ctx, method, url, body)
	if err != nil {
		return nil, err
	}
	// always json
	req.Header.Add("Content-Type", "application/json")
	return req, nil
}

func (c *Client) do(ctx context.Context, req *http.Request, ob interface{}) (*http.Response, error) {
	req = req.WithContext(ctx)
	client := c.HTTPClient
	if client == nil {
		client = http.DefaultClient
	}

	// todo: pagination

	for i := 0; i < c.MaxNetworkRetries; i++ {
		res, err := client.Do(req) // handling go routine?
		if err != nil {
			return nil, err
		}

		if res.StatusCode == 429 {
			select {
			case <-ctx.Done():
				return nil, ctx.Err()
			case <-time.After(200 * time.Millisecond):
				continue
			}
		}

		if res.StatusCode >= 300 {
			return nil, &APIError{
				StatusCode: res.StatusCode,
				Response:   res,
				Message:    "something wrong",
			}
		}

		if res.StatusCode == 200 {
			decoder := json.NewDecoder(res.Body)
			if err := decoder.Decode(ob); err != nil {
				return nil, &APIError{
					StatusCode: res.StatusCode,
					Response:   res,
					Message:    "decode error",
				}
			}
		}
		return res, nil
	}
	return nil, &APIError{
		StatusCode: 444,
		Response:   nil,
		Message:    "Exhausted all measurest",
	}
}

////////////////////////////////////////
type RawOutput struct {
	Response *http.Response `json:"-"`
}

func (o *RawOutput) ExiredAt() time.Time {
	var z time.Time
	_ = o.Response.Header.Get("xxx")
	return z
}

type PingInput struct {
	Message string
	Times   int
}
type PingOutput struct {
	Message string
	*RawOutput
}

func (c *Client) SendPing(ctx context.Context, input PingInput) (PingOutput, error) {
	method := "GET"
	url := path.Join(c.BasePath, "/ping")
	var output PingOutput

	req, err := c.newRequest(ctx, method, url, nil)
	if err != nil {
		return output, err
	}
	res, err := c.do(ctx, req, &output)
	if err != nil {
		return output, err
	}
	output.RawOutput = &RawOutput{Response: res}
	return output, nil
}
