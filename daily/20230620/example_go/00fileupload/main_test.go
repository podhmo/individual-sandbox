package main

import (
	"bytes"
	"fmt"
	"io"
	"mime/multipart"
	"net/http/httptest"
	"net/http/httputil"
	"os"
	"testing"
)

func TestUpload(t *testing.T) {
	filename := "hello.txt"
	content := bytes.NewBufferString("hello world")

	dir := t.TempDir()
	handler := Upload(dir)

	buf := new(bytes.Buffer)
	mw := multipart.NewWriter(buf)
	wf, err := mw.CreateFormFile("file", filename)
	if err != nil {
		t.Fatalf("unexpected file: %+v", err)
	}
	io.Copy(wf, content) // nolint
	mw.Close()

	req := httptest.NewRequest("POST", "/upload", buf)
	req.Header.Set("Content-Type", mw.FormDataContentType())

	rec := httptest.NewRecorder()
	handler(rec, req)

	// TODO: response check
	fmt.Fprintln(os.Stderr, "----------------------------------------")
	res := rec.Result()
	b, err := httputil.DumpResponse(res, true)
	fmt.Fprintln(os.Stderr, string(b), err)
	fmt.Fprintln(os.Stderr, "----------------------------------------")
}
