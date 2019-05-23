run:
	uvicorn myscript:app

client:
	echo '{"query":"{hello}"}' | http --json POST :8000/graphql/
