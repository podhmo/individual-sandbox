tdir = ./$(shell echo $@*/)

00:
	go clean $(tdir)
	CAPTURE_WRITEFILE=tracedata go test -v $(tdir) || :
	go clean $(tdir)
	CAPTURE_LAYOUT=json CAPTURE_WRITEFILE=tracedata2 go test -v $(tdir) || :
	tree $(tdir)

01:
	go clean $(tdir)
	CAPTURE_WRITEFILE=tracedata go test -v $(tdir) || :
	go clean $(tdir)
	CAPTURE_LAYOUT=json CAPTURE_WRITEFILE=tracedata2 go test -v $(tdir) || :
	tree $(tdir)

02:
	go clean $(tdir)
	CAPTURE_WRITEFILE=tracedata go test -v $(tdir) || :
	go clean $(tdir)
	CAPTURE_LAYOUT=json CAPTURE_WRITEFILE=tracedata2 go test -v $(tdir) || :
	tree $(tdir)

03:
	go clean $(tdir)
	CAPTURE_WRITEFILE=tracedata go test -v $(tdir) || :
	go clean $(tdir)
	CAPTURE_LAYOUT=json CAPTURE_WRITEFILE=tracedata2 go test -v $(tdir) || :
	tree $(tdir)

04:
	go clean $(tdir)
	CAPTURE_WRITEFILE=tracedata go test -v $(tdir) || :
	go clean $(tdir)
	CAPTURE_LAYOUT=json CAPTURE_WRITEFILE=tracedata2 go test -v $(tdir) || :
	tree $(tdir)

10:
	go test -v $(tdir)

11:
	go test -v $(tdir)
12:
	go test -v $(tdir)
