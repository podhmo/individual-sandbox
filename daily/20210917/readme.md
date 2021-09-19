## contextの対応どうすれば良いのか？

```go
func Do(w http.ResponseWriter, req *http.Request){
    ctx := req.Context()
    ctx = ctx.WithValue(traceIDKey, TraceID(req))
    Cont(ctx, body)
}
```

providerを作成するときにcontextはダメ。

## config的なもの

configは内部に持って置ける。

```go
type Provider struct {
    Config *Config
}

func (p *Provider) Session() (*Session, func(), error) {
    dburl := p.Config.DBURL
    session, err := NewSession(dburl)
    return session, session.Close, err
}
```

## session的なもの

cleanupとかが現れても全然問題なさそう。

```go
func Do(...) error{
    var session *Session
    {
        var err error
        session, cleanup, err = p.Session()
        if session != nil {
            defer cleanup()
        }
        if err != nil {
            return err
        }
    }
    Cont(ctx, session, ...)
}
```