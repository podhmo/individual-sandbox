PYTHON ?= ../bin/python

server:
	DEBUG=1 $(PYTHON) greeter_server.py
client:
	DEBUG=1 $(PYTHON) greeter_client.py

gen:
	$(PYTHON) -m grpc_tools.protoc -I. --python_out=. --grpc_python_out=. helloworld.proto

init: greeter_server.py greeter_client.py

greeter_server.py:
	touch $@
greeter_client.py:
	touch $@

