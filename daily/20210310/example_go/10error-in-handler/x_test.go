package expecterror

import (
	"errors"
	"io"
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestCloseConnection(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		c, _, err := w.(http.Hijacker).Hijack()
		if err != nil {
			t.Fatal(err)
		}
		t.Log("for test -- close connection")
		c.Close()
	}))
	defer ts.Close()

	_, err := http.Get(ts.URL)
	if err == nil {
		t.Errorf("error is expected but nil")
	}
	if !errors.Is(err, io.EOF) {
		t.Errorf("EOF is expected but return error is %[1]T, %+[1]v", err)
	}
}

// // panic: interface conversion: *httptest.ResponseRecorder is not http.Hijacker: missing method Hijack [recovered]
// //         panic: interface conversion: *httptest.ResponseRecorder is not http.Hijacker: missing method Hijack
// func TestErrorWithRecorder(t *testing.T) {
// 	rec := httptest.NewRecorder()
// 	handler := http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
// 		c, _, err := w.(http.Hijacker).Hijack()
// 		if err != nil {
// 			t.Fatal(err)
// 		}
// 		t.Log("for test -- close connection")
// 		c.Close()
// 	})

// 	handler(rec, httptest.NewRequest("GET", "http://example.net", nil))
// }
