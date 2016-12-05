package m

import (
	"bytes"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"net/url"
	"testing"

	"io/ioutil"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestGet(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		assert.Exactly(t, "/foo", r.URL.Path)
		assert.Exactly(t, "1", r.URL.Query().Get("value"))
	}))
	defer ts.Close()

	_, err := http.Get(ts.URL + "/foo?value=1")
	require.NoError(t, err)
}

func TestPost(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		assert.Exactly(t, "/foo", r.URL.Path)
		r.ParseForm()
		assert.Exactly(t, "1", r.Form.Get("value"))
	}))
	defer ts.Close()
	values := url.Values{}
	values.Add("value", "1")
	_, err := http.PostForm(ts.URL+"/foo", values)
	require.NoError(t, err)
}

func TestPostJSON(t *testing.T) {
	type data struct {
		Name string `json:"name"`
		Age  int    `json:"int"`
	}

	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		assert.Exactly(t, "/foo", r.URL.Path)
		var val data
		parseJSONRequest(r, func(b []byte) error {
			return json.Unmarshal(b, &val)
		})
		assert.Exactly(t, "foo", val.Name)
		assert.Exactly(t, 20, val.Age)
	}))
	defer ts.Close()

	dataset := data{
		Name: "foo",
		Age:  20,
	}

	b, err := json.Marshal(dataset)
	require.NoError(t, err)
	req, err := http.NewRequest("POST", ts.URL+"/foo", bytes.NewBuffer(b))
	req.Header.Set("Content-Type", "application/json")

	_, err = (&http.Client{}).Do(req)
	require.NoError(t, err)
}

func parseJSONRequest(r *http.Request, parse func(body []byte) error) error {
	if r.Body == nil {
		return errors.New("missing form body")
	}
	ct := r.Header.Get("Content-Type")
	if ct != "application/json" {
		return errors.Errorf("invalid content type: %v", ct)
	}
	b, err := ioutil.ReadAll(r.Body)
	if err != nil {
		return err
	}
	return parse(b)
}
