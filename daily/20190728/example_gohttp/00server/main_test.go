package main

import (
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/podhmo/go-webtest/jsonequal"
	"github.com/podhmo/go-webtest/snapshot"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func Test(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(Greeting))
	defer ts.Close()

	resp, err := http.Get(ts.URL)
	require.NoError(t, err)

	require.Exactly(t, http.StatusOK, resp.StatusCode)

	data, b, err := jsonequal.FromReadCloser(resp.Body)()
	require.NoError(t, err)

	want := snapshot.Take(t, data, snapshot.WithReplaceMap(map[string]interface{}{
		"#/now": data.(map[string]interface{})["now"],
	}))
	assert.NoError(t, jsonequal.ShouldBeSame(jsonequal.From(want), jsonequal.FromRawWithBytes(data, b)))
}
