run:
	uvicorn --debug app:app --port=5555

add: MESSAGE ?= hello world
add:
	echo '{"text": "${MESSAGE}", "completed": false}' | http --json POST :5555/notes
list:
	http --json :5555/notes
