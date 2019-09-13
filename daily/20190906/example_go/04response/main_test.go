package main

import (
	"bytes"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"os"
	"testing"

	"github.com/podhmo/noerror"
)

func Test(t *testing.T) {
	t.Run("empty", func(t *testing.T) {
		ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
		}))
		defer ts.Close()

		res, err := http.Get(ts.URL)
		noerror.Must(t, err)

		fmt.Println(res.ContentLength, res.Close, res.Body == http.NoBody, "------------------------------")
		io.Copy(os.Stderr, res.Body)
		fmt.Println(res.ContentLength, res.Close, res.Body == http.NoBody, "------------------------------")
		io.Copy(os.Stderr, res.Body)
		fmt.Println(res.ContentLength, res.Close, res.Body == http.NoBody, "------------------------------")
		io.Copy(os.Stderr, res.Body)
	})

	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
		w.Header().Set("Content-type", "application/json")
		fmt.Fprintln(w, `{"msg": "hello"}`)
	}))
	defer ts.Close()

	t.Run("normal", func(t *testing.T) {
		res, err := http.Get(ts.URL)
		noerror.Must(t, err)

		fmt.Println(res.ContentLength, res.Close, res.Body == http.NoBody, "------------------------------")
		io.Copy(os.Stderr, res.Body)
		fmt.Println(res.ContentLength, res.Close, res.Body == http.NoBody, "------------------------------")
		io.Copy(os.Stderr, res.Body)
		fmt.Println(res.ContentLength, res.Close, res.Body == http.NoBody, "------------------------------")
		io.Copy(os.Stderr, res.Body)
	})

	t.Run("clone", func(t *testing.T) {
		res, err := http.Get(ts.URL)
		noerror.Must(t, err)

		res2 := CloneResponse(res)
		fmt.Println(res.ContentLength, res.Close, res.Body == http.NoBody, "------------------------------")
		io.Copy(os.Stderr, res.Body)
		fmt.Println(res2.ContentLength, res2.Close, res2.Body == http.NoBody, "------------------------------")
		io.Copy(os.Stderr, res2.Body)
		fmt.Println(res2.ContentLength, res2.Close, res2.Body == http.NoBody, "------------------------------")
		io.Copy(os.Stderr, res2.Body)
	})

	t.Run("clone2", func(t *testing.T) {
		res, err := http.Get(ts.URL)
		noerror.Must(t, err)

		res2 := CloneResponse(res)
		fmt.Println(res2.ContentLength, res2.Close, res2.Body == http.NoBody, "------------------------------")
		io.Copy(os.Stderr, res2.Body)
		fmt.Println(res.ContentLength, res.Close, res.Body == http.NoBody, "------------------------------")
		io.Copy(os.Stderr, res.Body)
		fmt.Println(res.ContentLength, res.Close, res.Body == http.NoBody, "------------------------------")
		io.Copy(os.Stderr, res.Body)
	})
}

// CloneResponse has Side effect
func CloneResponse(res *http.Response) *http.Response {
	copied := *res
	if res.Body == http.NoBody {
		return &copied
	}
	var b bytes.Buffer
	res.Body = ioutil.NopCloser(io.TeeReader(res.Body, &b))
	copied.Body = ioutil.NopCloser(&b)
	return &copied
}
