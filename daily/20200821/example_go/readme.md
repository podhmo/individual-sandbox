## go clientのテストでもhttptest.Recorderを使いたい

使えるのでは？
あと、handlerを受け取れるようにするのでも良い。

あとstatic serveなども使える？

### 追記

```go
type HandlerRoundTripper struct {
	Before  func(*http.Request)
	Handler http.Handler
	After   func(*http.Response, *http.Request)
}

func (t *HandlerRoundTripper) RoundTrip(r *http.Request) (*http.Response, error) {
	if t.Before != nil {
		t.Before(r)
	}
	w := httptest.NewRecorder()
	t.Handler.ServeHTTP(w, r)
	res := w.Result()
	if t.After != nil {
		t.After(res, r)
	}
	return res, nil
}
```

例えば、こう。

### 追記

net/httpのhandler系のものは？

- FileSystem
- NewFileTransport()
- FileServerのjson抜きとかできる？

