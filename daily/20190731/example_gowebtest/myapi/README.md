```console
$ go test -v
=== RUN   TestIt
=== RUN   TestIt/200
body: map[string]interface {}{"message":"OK", "status":200}--- PASS: TestIt (0.00s)
    --- PASS: TestIt/200 (0.00s)
=== RUN   TestUnit
=== RUN   TestUnit/200
body: map[string]interface {}{"message":"OK", "status":200}--- PASS: TestUnit (0.00s)
    --- PASS: TestUnit/200 (0.00s)
PASS
ok  	myapi	0.004s
```
