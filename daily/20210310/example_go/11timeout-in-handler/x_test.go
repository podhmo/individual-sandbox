package expecterror

import (
	"errors"
	"net/http"
	"net/http/httptest"
	"testing"
	"time"
)

func TestTimeout(t *testing.T) {
	timeout := 100 * time.Millisecond
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		t.Logf("for test -- timeout forcely %v", 2*timeout)
		time.Sleep(timeout)
	}))
	defer ts.Close()

	client := &http.Client{Timeout: timeout}
	_, err := client.Get(ts.URL)
	if err == nil {
		t.Errorf("error is expected but nil")
	}

	// context.DeadlineExceeded あたりになってくれれば嬉しいのだけれど。。
	// &url.Error{Op:"Get", URL:"http://127.0.0.1:59233", Err:(*http.httpError)(0xc0001b2040)}
	var x interface{ Timeout() bool }
	if ok := errors.As(err, &x); !(ok && x.Timeout()) {
		t.Errorf("timeout is expected but return error is %[1]T, %#+[1]v", err)
	}
}
