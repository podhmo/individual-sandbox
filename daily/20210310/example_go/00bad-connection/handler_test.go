package handler

import (
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/podhmo/tenuki"
)

func TestIt(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		t.Log("got", r.URL)
		t.Logf("response %T, request %T", w, r)
		_, err := io.Copy(ioutil.Discard, r.Body)
		if err != nil {
			t.Fatal(err)
		}
		c, _, err := w.(http.Hijacker).Hijack()
		if err != nil {
			t.Fatal(err)
		}
		defer c.Close()
		fmt.Fprintln(c, "some bogus crap")
	}))
	defer ts.Close()

	f := tenuki.New(t)
	req := f.NewRequest("XXX", ts.URL, nil)
	res := f.Do(req)
	t.Logf("%+v", res)
}
