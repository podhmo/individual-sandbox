package expecterror

import (
	"errors"
	"fmt"
	"net/http"
	"testing"
	"time"
)

type errorTransport struct {
	T      *testing.T
	genErr func() error
}

func (t *errorTransport) RoundTrip(req *http.Request) (*http.Response, error) {
	err := t.genErr()
	t.T.Logf("error forcely %T", err)
	return nil, err
}

func TestErrorTransport(t *testing.T) {
	this := fmt.Errorf("*THIS*")
	transport := &errorTransport{T: t, genErr: func() error { return this }}

	client := &http.Client{Timeout: 100 * time.Millisecond, Transport: transport}
	_, err := client.Get("http://example.net.dummy") // テキトーなURL
	if err == nil {
		t.Errorf("error is expected but nil")
	}
	if !errors.Is(err, this) {
		t.Errorf("%#[2]v is expected but return error is %[1]T, %+[1]v", err, this)
	}

}
