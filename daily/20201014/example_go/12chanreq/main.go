package main

import (
	"context"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"strings"
	"time"
)

type Response struct {
	resCh     <-chan *http.Response
	errCh     <-chan error
	connectCh chan<- struct{}
}

func (r *Response) Next() (*http.Response, error) {
	r.connectCh <- struct{}{}

	select {
	case res := <-r.resCh:
		return res, nil
	case err := <-r.errCh:
		return nil, err
	}
}

func (r *Response) Close() {
	close(r.connectCh)
}

type Client struct {
}

func (c *Client) Do(ctx context.Context, method, path string) Response {
	resCh := make(chan *http.Response)
	errCh := make(chan error)
	connectCh := make(chan struct{}, 1)
	connectCh <- struct{}{}
	go func() {
		defer close(errCh)
		defer close(resCh)
		i := 0
		for {
			select {
			case <-ctx.Done():
				return
			case <-connectCh:
				req, err := http.NewRequestWithContext(ctx, method, path, strings.NewReader(fmt.Sprintf(`{"i": %d}`, i)))
				if err != nil {
					errCh <- err
					return
				}
				res, err := http.DefaultClient.Do(req)
				if err != nil {
					errCh <- err
					return
				}
				resCh <- res

				select {
				case <-ctx.Done():
					return
				case <-time.After(200 * time.Millisecond):
					i++
				}
			}
		}
	}()
	return Response{
		resCh:     resCh,
		errCh:     errCh,
		connectCh: connectCh,
	}
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
func run() error {
	c := &Client{}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	conn := c.Do(ctx, "POST", "https://httpbin.org/post")
	defer conn.Close()

	for i := 0; i < 3; i++ {
		res, err := conn.Next()
		if err != nil {
			fmt.Println("hmm", err)
			break
		}
		defer res.Body.Close()
		log.Println("got")
		io.Copy(os.Stdout, res.Body)
	}
	return nil
}
