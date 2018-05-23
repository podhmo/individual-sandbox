この記事を真似して作ってみたやつ

- https://medium.com/statuscode/how-i-write-go-http-services-after-seven-years-37c208122831

```console
$ make
go test -v
=== RUN   TestAboutIntegration
{
  "version": "1.0"
}
--- PASS: TestAboutIntegration (0.00s)
=== RUN   TestGreet
=== RUN   TestGreet/no-json_404
=== RUN   TestGreet/json_200
--- PASS: TestGreet (0.00s)
    --- PASS: TestGreet/no-json_404 (0.00s)
    --- PASS: TestGreet/json_200 (0.00s)
PASS
```
