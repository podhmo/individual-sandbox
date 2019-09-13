## go 何度も消費可能なResponse

- closed?
- Body
- ContentLength?

net/http/httputilを参考にすれば良さそうな気がするけれど

あと NoBodyを気にするかどうか

```go
var NoBody = noBody{}

type noBody struct{}

func (noBody) Read([]byte) (int, error)         { return 0, io.EOF }
func (noBody) Close() error                     { return nil }
func (noBody) WriteTo(io.Writer) (int64, error) { return 0, nil }

var (
	// verify that an io.Copy from NoBody won't require a buffer:
	_ io.WriterTo   = NoBody
	_ io.ReadCloser = NoBody
)
```

## go io系のやつら

- io.Pipe
- io.TeeReader
- io.MultiWriter
