package main

import (
	"fmt"
	"io"
	"log"
	"net/http"
	"net/http/httptest"
	"net/http/httputil"
	"net/url"
	"os"
	"strings"
	"sync"
	"time"

	uuid "github.com/satori/go.uuid"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	upstream := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		time.Sleep(500 * 1000 * time.Microsecond)
		fmt.Fprintf(w, `{"message": "hello"}\n`)
	}))
	defer upstream.Close()

	u, err := url.Parse(upstream.URL)
	if err != nil {
		return err
	}
	proxy := NewSingleHostReverseProxy(u)
	ts := httptest.NewServer(proxy)
	defer ts.Close()

	res, err := http.Get(ts.URL)
	if err != nil {
		return err
	}

	if _, err := io.Copy(os.Stdout, res.Body); err != nil {
		return err
	}
	defer res.Body.Close()
	return nil
}

func NewSingleHostReverseProxy(target *url.URL) *httputil.ReverseProxy {
	targetQuery := target.RawQuery
	timeMap := map[string]time.Time{}
	var mu sync.Mutex

	director := func(req *http.Request) {
		tracingID := uuid.NewV4().String()
		req.Header.Set("Tracing-Id", tracingID)
		start := time.Now()
		mu.Lock()
		timeMap[tracingID] = start
		mu.Unlock()

		req.URL.Scheme = target.Scheme
		req.URL.Host = target.Host
		req.URL.Path = singleJoiningSlash(target.Path, req.URL.Path)
		if targetQuery == "" || req.URL.RawQuery == "" {
			req.URL.RawQuery = targetQuery + req.URL.RawQuery
		} else {
			req.URL.RawQuery = targetQuery + "&" + req.URL.RawQuery
		}
		if _, ok := req.Header["User-Agent"]; !ok {
			// explicitly disable User-Agent so it's not set to default value
			req.Header.Set("User-Agent", "")
		}
	}
	return &httputil.ReverseProxy{
		Director: director,
		ModifyResponse: func(res *http.Response) error {
			req := res.Request
			tracingID := req.Header.Get("Tracing-Id")
			end := time.Now()

			var duration time.Duration
			mu.Lock()
			// TODO: remove orphan
			start, ok := timeMap[tracingID]
			if ok {
				duration = end.Sub(start)
				delete(timeMap, tracingID)
			}
			mu.Unlock()
			log.Printf("duration:%s\ttracingID:%s\tcode:%d\tmethod:%s\turl:%s",
				duration, tracingID, res.StatusCode, req.Method, req.URL,
			)
			return nil
		},
	}
}

func singleJoiningSlash(a, b string) string {
	aslash := strings.HasSuffix(a, "/")
	bslash := strings.HasPrefix(b, "/")
	switch {
	case aslash && bslash:
		return a + b[1:]
	case !aslash && !bslash:
		return a + "/" + b
	}
	return a + b
}
