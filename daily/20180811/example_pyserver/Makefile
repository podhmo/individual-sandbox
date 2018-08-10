setup:
	pip install -r requirements.txt
server:
	python server.py --port=5000
server2:
	python server2.py --port=5000
server3:
	python server3.py --port=5000
proxy:
	python -m reqtrace proxy.py --port=5001
proxy2:
	python -m reqtrace proxy2.py --port=5001
client:
	http --json post :5001 foo:bar xxx==zzz x=z
client2:
	http --form post :5001 foo:bar xxx==zzz x=z
