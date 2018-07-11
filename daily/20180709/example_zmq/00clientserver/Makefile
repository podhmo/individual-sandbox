default:
	rm -f server.pid
	python server.py &
	echo $! > server.pid
	python client.py
	kill `cat server.pid` && rm server.pid

client:
	python client.py &
	echo $$! > client.pid

setup:
	pip install -r requirements.txt

