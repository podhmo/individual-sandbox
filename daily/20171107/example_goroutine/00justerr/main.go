package main

// instead of sync.WaitGroup
import (
	"fmt"
	"net/http"

	"golang.org/x/sync/errgroup"
)

var g errgroup.Group
var urls = []string{
	"http://www.golang.org/",
	"http://www.google.com/",
	"http://www.somestupidname.com/",
}

func main() {
	for _, url := range urls {
		// Launch a goroutine to fetch the URL.
		url := url // https://golang.org/doc/faq#closures_and_goroutines
		g.Go(func() error {
			// Fetch the URL.
			fmt.Println("requesting...", url)
			resp, err := http.Get(url)
			if err == nil {
				resp.Body.Close()
			}
			fmt.Println("returned", url)
			return err
		})
	}
	// Wait for all HTTP fetches to complete.
	if err := g.Wait(); err == nil {
		fmt.Println("Successfully fetched all URLs.")
	}
}
