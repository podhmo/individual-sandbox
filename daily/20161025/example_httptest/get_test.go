package example_httptest

import (
	"fmt"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"testing"
)

// client success but status code is 500
func Test500(t *testing.T) {
	handler := http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
		w.WriteHeader(500)
	})
	ts := httptest.NewServer(handler)
	response, _, err := callHTTP(ts.URL)
	if err != nil {
		t.Fatal(err)
	}
	if response.StatusCode != 500 {
		t.Errorf("expected: 500, actual:%d", response.StatusCode)
	}
}

// client success and status code is 200
func Test200(t *testing.T) {
	handler := http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
		fmt.Fprintln(w, "hello")
	})
	ts := httptest.NewServer(handler)
	response, text, err := callHTTP(ts.URL)
	if err != nil {
		t.Fatal(err)
	}
	if response.StatusCode != 200 {
		t.Errorf("expected: 200, actual:%d", response.StatusCode)
	}
	if text != "hello\n" {
		t.Errorf(`expected: "hello\n", actual:%q`, text)
	}
}

// client failure
func TestNG(t *testing.T) {
	handler := http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
		fmt.Fprintln(w, "hello")
	})
	ts := httptest.NewServer(handler)
	req, err := http.NewRequest("GET", ts.URL, nil)
	if err != nil {
		t.Fatal(err)
	}
	client := &http.Client{}
	response, err := client.Do(req)
	defer response.Body.Close()

	b, err := ioutil.ReadAll(response.Body)
	if err != nil {
		t.Fatal(err)
	}

	fmt.Println("---", string(b))
}

func callHTTP(url string) (*http.Response, string, error) {
	response, err := http.Get(url)
	if err != nil {
		return nil, "", err
	}
	defer response.Body.Close()
	b, err := ioutil.ReadAll(response.Body)
	if err != nil {
		return nil, "", err
	}
	return response, string(b), err
}
