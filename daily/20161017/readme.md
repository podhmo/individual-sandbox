# jwt jwtの便利なサイト

- [JSON Web Tokens - jwt.io](https://jwt.io/)

# http statusのリダイレクトのやつ

明示的にresource参照してほしい場合には 303(See Other)を使うべき。

- [302 Found / 303 See Other / 307 Temporary Redirect](https://gist.github.com/doloopwhile/e808ddafe76f0dcefeee)

# golang pretty printer的なやつ

- [davecgh/go-spew: Implements a deep pretty printer for Go data structures to aid in debugging](https://github.com/davecgh/go-spew)

```
go get -u github.com/davecgh/go-spew/spew
```

```go
spew.Dump(myVar1, myVar2, ...)
```

# go http関係のテストの復習

[net/http/httptest](https://golang.org/pkg/net/http/httptest/) を使う。

- `httptest.NewServer`, `httptest.NewTLSServer` を使うもの
- `httptest.NewRecorder` を使うもの

NewServer

```go
var sampleHandler = http.HandlerFunc(func (w http.ResponseWriter, r *http.Request) {
    fmt.Fprintf(w, "Hello HTTP Test")
})

ts := httptest.NewServer(sampleHandler)
response, err := http.Get(ts.URL)

// 後は通常と同様の処理
```


NewRecorder

```go
response := httptest.NewRecorder()
request, _ := http.NewRequestuest("GET", uri, nil)
// http.DefaultServeMux.ServeHTTP(response, request)
sampleHandler.ServeHTTP(response, request)
// response.Bodyを見る
```

参考

- [How to test http calls in go using httptest - Stack Overflow](http://stackoverflow.com/questions/16154999/how-to-test-http-calls-in-go-using-httptest)
- [Testing Your (HTTP) Handlers in Go · go, web, go](https://elithrar.github.io/article/testing-http-handlers-go/)

# go `err != err` を防ぐ方法

ひどい。 `err != err` は常に偽。

```go
func f() (X, error) {
	x, err := g()
    if err != err {
		return nil, err
	}
    do_something(x)
    return x, nil
}
```
