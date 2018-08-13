server:
	python server.py --port 5000
proxy:
	python proxy.py --port 5001
client:
	http :5001/api/foo
client2:
	echo '{"foo": "boo"}' | http --json POST :5001 headerX:headerV qsK==qsV
client3:
	http --form POST :5001 headerX:headerV qsK==qsV foo=boo
