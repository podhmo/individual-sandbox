run: myuuid.so
	LD_LIBRARY_PATH=. python -m memory_profiler main.py

myuuid.so: myuuid.go
	go build -o $@ --buildmode c-shared .

