00:
	python 00*.py
01:
	python 01*.py
02:
	python 02*.py
03:
	mypy --strict 03*.py
	python 03*.py

see-docs: OPEN ?= $(shell which xdg-open 2>/dev/null || open)
see-docs:
	$(OPEN) http://localhost:8080/docs
	# http :8080/openapi.json

gen-lock:
	cat requirements.txt | pipdeptidy > requirements.lock
