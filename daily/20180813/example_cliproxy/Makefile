00:
	python 00*.py
	HTTPS_PROXY=localhost:4444 HTTP_PROXY=localhost:4444 python 00*.py

01:
	python 01*.py
	HTTPS_PROXY=localhost:4444 HTTP_PROXY=localhost:4444 python 01*.py
02:
	python 02*.py
	HTTP_PROXY=localhost:4444 python 02*.py

proxy:
	python -m reqtrace proxy.py --port=4444

clean:
	rm -rf roundtrips
