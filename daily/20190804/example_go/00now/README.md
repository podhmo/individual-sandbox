OK

```console
$ go test -v
=== RUN   TestIt
--- PASS: TestIt (0.00s)
    snapshot.go:56: load testdata: "testdata/TestIt.golden"
PASS
ok  	m	0.005s
```

NG

```console
$ go test -v
=== RUN   TestIt
--- FAIL: TestIt (0.00s)
    snapshot.go:56: load testdata: "testdata/TestIt.golden"
    main_test.go:35: unexpected error, not equal json
        left:
        	{"message":"bye","now":"2019-08-04T21:40:07+09:00"}
        
        right:
        	{"message":"hello","now":"2019-08-04T21:40:07+09:00"}
        on equal check
FAIL
exit status 1
FAIL	m	0.005s
```

NG2

```console
go test -v
=== RUN   TestIt
--- FAIL: TestIt (0.00s)
    main_test.go:32: unexpected error, status code, expected 200, but actual 201
         response: {"message":"hello","now":"2019-08-04T21:41:13+09:00"}
        
FAIL
exit status 1
FAIL	m	0.005s
```
