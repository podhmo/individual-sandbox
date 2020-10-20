package main

import (
	"net/http"
	"net/http/httptest"
	"strconv"
	"testing"

	"github.com/podhmo/tenuki"
)

func Handler(w http.ResponseWriter, r *http.Request) {
	q := r.URL.Query()
	var input struct {
		limit  int
		cursor int
	}
	var err error
	if v := q.Get("limit"); v != "" {
		input.limit, err = strconv.Atoi(v)
		if err != nil {
			tenuki.Render(w, r).JSON(400, err.Error())
			return
		}
	}
	if v := q.Get("cursor"); v != "" {
		input.cursor, err = strconv.Atoi(v)
		if err != nil {
			tenuki.Render(w, r).JSON(400, err.Error())
			return
		}
	}

	xs := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11}
	N := len(xs)
	if input.cursor > 0 {
		if input.cursor > len(xs) {
			xs = []int{}
		} else {
			xs = xs[input.cursor:]
		}
	}
	if input.limit > 0 {
		if input.limit <= len(xs) {
			xs = xs[:input.limit]
		}
	}

	q = r.URL.Query()
	q.Set("cursor", strconv.Itoa(input.cursor+len(xs)))
	nextURL := r.URL.Host + r.URL.Path + "?" + q.Encode()
	tenuki.Render(w, r).JSON(200, map[string]interface{}{
		"items":   xs,
		"next":    nextURL,
		"hasNext": input.cursor+len(xs) < N,
	})
}

func TestIt(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(Handler))

	f := tenuki.New(t)
	{
		req := f.NewRequest("GET", ts.URL+"?cursor=1&limit=3", nil)
		res := f.Do(req)
		got := map[string]interface{}{}
		f.Extract().JSON(res, &got)
	}
	{
		req := f.NewRequest("GET", ts.URL+"?cursor=4&limit=3", nil)
		res := f.Do(req)
		got := map[string]interface{}{}
		f.Extract().JSON(res, &got)
	}
	{
		req := f.NewRequest("GET", ts.URL+"?cursor=7&limit=3", nil)
		res := f.Do(req)
		got := map[string]interface{}{}
		f.Extract().JSON(res, &got)
	}
	{
		req := f.NewRequest("GET", ts.URL+"?cursor=10&limit=3", nil)
		res := f.Do(req)
		got := map[string]interface{}{}
		f.Extract().JSON(res, &got)
	}
}
