# golang 30分後など取得する

```go
now := time.Now()
now.Add(30 * time.Minute)
```

# golang 日時をてきとうに文字列から作る

```go
t, err := time.Parse(time.RFC3339, "2016-11-02T00:00:00Z")
// 2016-11-02 00:00:00 UTC
```
