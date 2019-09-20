package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"net/http/httputil"
	"os"
	"testing"
)

func TestIt(t *testing.T) {
	handler := func(w http.ResponseWriter, req *http.Request) {
		b, err := httputil.DumpRequest(req, true)
		if err != nil {
			t.Fatal(err)
		}
		fmt.Println("========================================")
		fmt.Println(string(b))
		fmt.Println("========================================")
		fmt.Fprintln(w, "ok")
	}
	ts := httptest.NewServer(http.HandlerFunc(handler))
	defer ts.Close()

	var buf bytes.Buffer
	encoder := json.NewEncoder(&buf)
	encoder.Encode(`{"xxx": "yyyy"}`)
	req, err := http.NewRequest("POST", ts.URL, &buf)
	if err != nil {
		t.Fatal(err)
	}
	res, err := http.DefaultClient.Do(req)
	if err != nil {
		t.Fatal(err)
	}

	fmt.Println("----------------------------------------")
	defer res.Body.Close()
	io.Copy(os.Stdout, res.Body)
	fmt.Println("----------------------------------------")
}
