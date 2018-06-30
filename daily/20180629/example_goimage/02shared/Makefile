default: libsize.so
	$(MAKE) -C ../
	python main.py --filename=../images/cat.png

show: libsize.so
	nm --defined-only -g libsize.so

libsize.so: libsize.go
	GODEBUG=cgocheck=2 go build -o $@ --buildmode c-shared $^
