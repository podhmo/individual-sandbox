default: libsize.so
	$(MAKE) -C ../
	# LD_TRACE_LOADED_OBJECTS=1 LD_VERBOSE=1 LD_WARN=1 python main.py --filename=../images/cat.png
	LD_LIBRARY_PATH=. python main.py --filename=../images/cat.png

clean:
	rm -vf *.so *.c *.o

show: libsize.so
	nm --defined-only -g libsize.so

libsize.so: libsize.go
	GODEBUG=cgocheck=2 go build -o $@ --buildmode c-shared $^
