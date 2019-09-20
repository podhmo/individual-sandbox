package bauthclient

import "net/http"

func New() *Client {
	return &Client{Client: &http.Client{}}
}

type Client struct {
	Client *http.Client
}

func (c *Client) Do() (*http.Response, error) {
	req, err := http.NewRequest("GET", "/", nil)
	if err != nil {
		return nil, err
	}
	req.SetBasicAuth("user", "pass")
	return c.Client.Do(req)
}
