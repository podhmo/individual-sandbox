##

```console
$ go test -bench . -benchmem
$ go test -bench . -benchmem -memprofile=x.prof
$ go tool pprof x.prof
> top
> web
```
