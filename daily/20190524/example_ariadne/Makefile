run:
	uvicorn myscript:app

client:
	curl 'http://localhost:8000/' -H 'Accept-Encoding: gzip, deflate, br' -H 'Content-Type: application/json' -H 'Accept: application/json' -H 'Connection: keep-alive' -H 'DNT: 1' -H 'Origin: http://localhost:8000' --data-binary '{"query":"{hello}"}' --compressed

client2:
	echo '{"query":"{hello}"}' | http --json POST :8000

setup:
	pip install ariadne uvicorn --upgrade
