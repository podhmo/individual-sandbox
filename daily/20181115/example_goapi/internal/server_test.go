package internal

import (
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestHandler(t *testing.T) {
	r := httptest.NewRequest("GET", "/", nil)
	rec := httptest.NewRecorder()

	NewServer().Handler().ServeHTTP(rec, r)

	if rec.Code != http.StatusOK {
		t.Errorf("expected %d, but %d", http.StatusOK, rec.Code)
	}
	decoder := json.NewDecoder(rec.Body)

	var body map[string]interface{}
	if err := decoder.Decode(&body); err != nil {
		t.Fatalf("broken response %v", err)
	}
}

func TestServer(t *testing.T) {
	ts := httptest.NewServer(NewServer().Handler())
	defer ts.Close()

	client := http.DefaultClient // xxx
	req, err := http.NewRequest("GET", ts.URL+"/", nil)
	if err != nil {
		t.Fatal(err)
	}
	resp, err := client.Do(req)
	if err != nil {
		t.Fatal(err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Errorf("expected %d, but %d", http.StatusOK, resp.StatusCode)
	}
	decoder := json.NewDecoder(resp.Body)

	var body map[string]interface{}
	if err := decoder.Decode(&body); err != nil {
		t.Fatalf("broken response %v", err)
	}
}
