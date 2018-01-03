00:
	{ python 00hello.py & echo $$! > pid; }
	http :8000/ -b | tee 00.output
	kill `cat pid` && rm pid
