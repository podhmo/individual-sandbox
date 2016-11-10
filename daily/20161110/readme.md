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

# golang nowをmockできるようにするやつ

これなんでこんなに大変なんだろ。。

```go
nowgen := NewMockNowGen()
ModifyWith(nowgen,
    func(m map[NowGenUseCase]time.Time) {
        fmt.Println("before mock", nowgen.Now())
        mockNow, _ := time.Parse(time.RFC3339, "2000-01-01T00:00:00Z")
        m[NowGenUseCaseNow] = mockNow
        fmt.Println("after mock", nowgen.Now())
    }, true)
fmt.Println("after clear", nowgen.Now())
```
