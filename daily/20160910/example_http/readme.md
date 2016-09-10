- 00 単純な文字列を返す http server
- 01 json responseを返す http server (雑なtest付き)

# http packageの使い方を整理していく

## hello server

とりあえず `net/http` を使う。

```go
http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
    fmt.Fprintf(w, "Hello, %q", html.EscapeString(r.URL.Path))
})

log.Fatal(http.ListenAndServe(":8080", nil))
```


