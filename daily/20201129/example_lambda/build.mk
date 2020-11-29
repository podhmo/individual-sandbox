# build

dst/hello: 00hello/main.go
	GOOS=linux go build -o $@ $<
dst/hello.zip: dst/hello
	zip $@ $<

dst/echo: 01echo/main.go
	GOOS=linux go build -o $@ $<
dst/echo.zip: dst/echo
	zip $@ $<

dst/pp: 02pp/main.go
	GOOS=linux go build -o $@ $<
dst/pp.zip: dst/pp
	zip $@ $<
