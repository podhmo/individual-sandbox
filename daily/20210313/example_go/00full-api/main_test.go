package main

import (
	"net/http/httptest"
	"testing"

	"github.com/podhmo/tenuki"
	"github.com/podhmo/tenuki/difftest"
)

func TestWelcome(t *testing.T) {
	ts := httptest.NewServer(setupServer())
	defer ts.Close()
	f := tenuki.New(t)
	req := f.NewRequest("GET", ts.URL+"/", nil)
	res := f.Do(req,
		tenuki.AssertStatus(200),
	)

	got := f.Extract().JSON(res)
	want := `{"version": "0.0.0", "message": "welcome! this is foo."}`
	difftest.AssertGotAndWantString(t, got, want)
}
