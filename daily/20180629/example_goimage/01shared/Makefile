default: libsize.so
	$(MAKE) -C ../
	python main.py --filename=../images/cat.png

show: libsize.so
	nm --deffined-only -g libsize.so

libsize.so: libsize.go
	go build -o $@ --buildmode c-shared $^
