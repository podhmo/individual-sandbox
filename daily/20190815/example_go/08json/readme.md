```console
$ go run main.go
2019/08/16 01:21:56 main.go:152: x	error:{"father":{"name":"required"},"mother":{"name":"required"}}
```

benchmark

```
$ go test -bench . -benchmem
goos: linux
goarch: amd64
pkg: m/08json
BenchmarkNormalOK-4   	2000000000	         0.00 ns/op	       0 B/op	       0 allocs/op
BenchmarkWrapOK-4     	2000000000	         0.00 ns/op	       0 B/op	       0 allocs/op
BenchmarkNormalNG-4   	2000000000	         0.00 ns/op	       0 B/op	       0 allocs/op
BenchmarkWrapNG-4     	2000000000	         0.00 ns/op	       0 B/op	       0 allocs/op
PASS
```
