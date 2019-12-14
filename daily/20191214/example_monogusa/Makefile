00:
	python -m monogusa.cli $(shell echo $@*/)commands.py hello --name=world
	python -m monogusa.cli $(shell echo $@*/)commands.py bye --name=world

01:
	python $@*/web.py --debug --port 44444
01-hello:
	echo '{"name": "world"}' | http --json POST :44444/hello
01-bye:
	echo '{"name": "world"}' | http --json POST :44444/bye
01-docs:
	python -m webbrowser -t http://localhost:44444/docs
01-show-doc:
	python 01*/web.py --show-doc
