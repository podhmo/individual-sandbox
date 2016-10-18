## `nil != nil`

```
$ make zero
go vet 00nilnil.go
golint 00nilnil.go
go run 00nilnil.go
# command-line-arguments
./00nilnil.go:15: invalid operation: nil != nil (operator != not defined on nil)
make: *** [zero] Error 2
```

## `err != err`

```
$ make one
go vet 01errerr.go
golint 01errerr.go
go run 01errerr.go
2016/10/18 21:30:02 don't call <nil>
```
