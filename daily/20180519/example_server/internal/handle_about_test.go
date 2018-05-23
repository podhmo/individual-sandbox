package internal

import (
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"testing"
)

func TestAboutIntegration(t *testing.T) {
	// using actual server
	ts := httptest.NewServer(newServer())
	defer ts.Close()

	resp, err := http.Get(fmt.Sprintf("%s/about", ts.URL))
	if err != nil {
		t.Fatal("unrequired", err)
	}

	io.Copy(os.Stdout, resp.Body)
}
