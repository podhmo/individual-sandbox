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

02:
	python $@*/web.py --debug --port 44444
02-hello:
	echo '{"name": "world"}' | http --json POST :44444/hello
02-bye:
	echo '{"name": "world"}' | http --json POST :44444/bye
02-docs:
	python -m webbrowser -t http://localhost:44444/docs
02-show-doc:
	python 02*/web.py --show-doc
