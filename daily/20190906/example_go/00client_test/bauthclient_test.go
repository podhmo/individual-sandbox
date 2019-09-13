package bauthclient

import (
	"bytes"
	"io"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"net/url"
	"os"
	"testing"

	"github.com/podhmo/noerror"
)

type TripFunc func(*http.Request) (*http.Response, error)

func (f TripFunc) RoundTrip(req *http.Request) (*http.Response, error) {
	return f(req)
}

func assertReq(t *testing.T, req *http.Request) {
	t.Helper()
	user, pass, ok := req.BasicAuth()
	noerror.Must(t, noerror.Equal(true).Actual(ok))
	noerror.Must(t, noerror.Equal("user").Actual(user))
	noerror.Must(t, noerror.Equal("pass").Actual(pass))
}

func TestClient(t *testing.T) {
	t.Run("roundTripper", func(t *testing.T) {
		c := New()
		c.Client.Transport = TripFunc(func(req *http.Request) (*http.Response, error) {
			assertReq(t, req)
			return &http.Response{
				StatusCode: 200,
				Body:       ioutil.NopCloser(bytes.NewBufferString("")),
				Header:     http.Header{},
			}, nil
		})
		res, err := c.Do()
		noerror.Must(t, err)
		io.Copy(os.Stdout, res.Body)
		defer res.Body.Close()
	})

	t.Run("handler", func(t *testing.T) {
		c := New()
		c.Client.Transport = TripFunc(func(req *http.Request) (*http.Response, error) {
			w := httptest.NewRecorder()
			func(w http.ResponseWriter, req *http.Request) {
				assertReq(t, req)
			}(w, req)
			return w.Result(), nil
		})
		res, err := c.Do()
		noerror.Must(t, err)
		io.Copy(os.Stdout, res.Body)
		defer res.Body.Close()
	})

	t.Run("server", func(t *testing.T) {
		c := New()
		ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
			assertReq(t, req)
		}))
		defer ts.Close()
		c.Client.Transport = TripFunc(func(req *http.Request) (*http.Response, error) {
			u, err := url.Parse(ts.URL)
			if err != nil {
				return nil, err
			}
			req.URL = u
			return http.DefaultTransport.RoundTrip(req)
		})
		res, err := c.Do()
		noerror.Must(t, err)
		io.Copy(os.Stdout, res.Body)
		defer res.Body.Close()
	})
}
