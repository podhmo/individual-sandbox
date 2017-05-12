package main

import (
	"context"
	"io"
	"log"
	"net/http"
	"os"

	"github.com/k0kubun/pp"
)

// Transport :
type Transport struct {
	Base http.RoundTripper
}

// RoundTrip :
func (t *Transport) RoundTrip(req *http.Request) (*http.Response, error) {
	if sc := GetCollectorFromContext(req.Context()); sc != nil {
		sc.Collect(req)
	}
	return t.Base.RoundTrip(req)
}

func tick(ctx context.Context, client *http.Client, url string) error {
	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return err
	}
	req = req.WithContext(ctx)
	response, err := client.Do(req)
	if err != nil {
		return err
	}
	defer response.Body.Close()
	io.Copy(os.Stdout, response.Body)
	return nil
}

// StatCollector :
type StatCollector struct {
	Stat map[string]int
}

// NewStatCollector :
func NewStatCollector() *StatCollector {
	return &StatCollector{Stat: map[string]int{}}
}

// Collect :
func (sc *StatCollector) Collect(req *http.Request) {
	sc.Stat[req.Host]++
}

// GetCollectorFromContext :
func GetCollectorFromContext(ctx context.Context) *StatCollector {
	switch sc := ctx.Value(K).(type) {
	case *StatCollector:
		return sc
	default:
		return nil
	}
}

// K :
var K struct{}

func main() {
	client := &http.Client{Transport: &Transport{Base: http.DefaultTransport}}
	sc := NewStatCollector()
	ctx := context.Background()
	ctx = context.WithValue(ctx, K, sc)
	if err := tick(ctx, client, "http://localhost:54321/foo"); err != nil {
		log.Fatal(err)
	}
	if err := tick(ctx, client, "http://localhost:54321/foo"); err != nil {
		log.Fatal(err)
	}
	if err := tick(ctx, client, "http://localhost:54321/bar"); err != nil {
		log.Fatal(err)
	}
	pp.Print(sc.Stat)
}
