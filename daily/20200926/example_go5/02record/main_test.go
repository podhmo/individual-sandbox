package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"testing"
)

func Handler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "hello world")
}

// TestUseRecord 通常のhttptest.Recorderを使う例
func TestUseRecord(t *testing.T) {
	rec := httptest.NewRecorder()
	req := httptest.NewRequest("", "http:", nil)
	Handler(rec, req)

	res := rec.Result()
	if want, got := http.StatusOK, res.StatusCode; want != got {
		t.Errorf("status code\nwant\n\t%d\nbut\n\t%d", want, got)
	}

	got, _ := ioutil.ReadAll(res.Body)
	if want := "hello world"; want != string(got) {
		t.Errorf("body\nwant\n\t%+v\nbut\n\t%+v", want, got)
	}
}

// このRoundTripperを利用したい

type EchoURL struct {
	T         *testing.T
	Transport http.RoundTripper
}

func (t *EchoURL) RoundTrip(req *http.Request) (*http.Response, error) {
	t.T.Helper()
	t.T.Logf("request: %v", req.URL)
	tranport := t.Transport
	if tranport == nil {
		tranport = http.DefaultTransport
	}
	return tranport.RoundTrip(req)
}

// TestUseTestServer テストサーバーを使う場合は本物の通信を行う。Transportを見てくれる。
func TestUseTestServer(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(Handler))
	defer ts.Close()

	client := &http.Client{Transport: &EchoURL{T: t}}
	res, err := client.Get(ts.URL)
	if err != nil {
		t.Fatalf("res: %+v", err)
	}

	if want, got := http.StatusOK, res.StatusCode; want != got {
		t.Errorf("status code\nwant\n\t%d\nbut\n\t%d", want, got)
	}

	got, _ := ioutil.ReadAll(res.Body)
	if want := "hello world"; want != string(got) {
		t.Errorf("body\nwant\n\t%+v\nbut\n\t%+v", want, got)
	}
}

type HandlerTripper struct {
	Rec     *httptest.ResponseRecorder
	Handler http.HandlerFunc
}

func (t *HandlerTripper) RoundTrip(req *http.Request) (*http.Response, error) {
	t.Handler(t.Rec, req)
	return t.Rec.Result(), nil
}

// TestUseRecordWithTransport そういうTransportを書いてあげれば済む話だ
func TestUseRecordWithTransport(t *testing.T) {
	rec := httptest.NewRecorder()
	client := &http.Client{
		Transport: &EchoURL{
			T:         t,
			Transport: &HandlerTripper{Rec: rec, Handler: Handler},
		},
	}
	res, err := client.Get("http:")
	if err != nil {
		t.Fatalf("res: %+v", err)
	}

	if want, got := http.StatusOK, res.StatusCode; want != got {
		t.Errorf("status code\nwant\n\t%d\nbut\n\t%d", want, got)
	}

	got, _ := ioutil.ReadAll(res.Body)
	if want := "hello world"; want != string(got) {
		t.Errorf("body\nwant\n\t%+v\nbut\n\t%+v", want, got)
	}
}
