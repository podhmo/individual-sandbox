goaの出力するコードってどんなだったっけ？ということを思い出してみる。

## install

```console
$ go mod init m
$ GO111MODULE=on go get -u goa.design/goa/v3/...@v3
$ mkdir design
```

## write design

write design https://github.com/goadesign/goa#1-design

## gen

```console
$ goa gen m/design
# 初回だけexampleを生成してscaffold
$ goa example m/design
```

## impl

```diff
diff --git a/daily/20200926/example_goa/calc.go b/daily/20200926/example_goa/calc.go
index 55e27b5e..cd98a9f4 100644
--- a/daily/20200926/example_goa/calc.go
+++ b/daily/20200926/example_goa/calc.go
@@ -20,5 +20,5 @@ func NewCalc(logger *log.Logger) calc.Service {
 // Add implements add.
 func (s *calcsrvc) Add(ctx context.Context, p *calc.AddPayload) (res int, err error) {
 	s.logger.Print("calc.add")
-	return
+	return p.A + p.B, nil
 }
```

## run

```console
$ go run cmd/calc/*.go --help
$ go run cmd/calc/*.go --debug --http-port 44444
[calcapi] 11:00:27 HTTP "Add" mounted on GET /add/{a}/{b}
[calcapi] 11:00:27 HTTP server listening on "localhost:44444"
[calcapi] 11:01:11 id=5hmKqvlC req=GET /add/100/10 from=127.0.0.1
```

```console
$ go run cmd/calc-cli/*.go --help
$ go run cmd/calc-cli/*.go --url http://localhost:44444 calc add -a 100 -b 10
110
```
