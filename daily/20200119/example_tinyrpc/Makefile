00:
	ioknife too --cmd "make $@-server" --cmd "make $@-client"
00-server:
	python 00zmq_server_example.py
00-client:
	python 00zmq_client_example.py

01:
	pycomment --inplace $(shell echo $@*.py)
02:
	pycomment --inplace $(shell echo $@*.py)
03:
	pycomment --inplace $(shell echo $@*.py)
04:
	LOGGING_TIME=relative DEBUG=1 python $(shell echo $@*.py) run_sequentially
	LOGGING_TIME=relative DEBUG=1 python $(shell echo $@*.py) run_concurrently
05:
	python $(shell echo $@*.py)
06:
	python $(shell echo $@*.py)
07:
	DEBUG=1 python $(shell echo $@*.py) manager
08:
	DEBUG=1 python $(shell echo $@*.py) manager

setup:
	pip install -r requirements.txt
