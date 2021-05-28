PYTHON ?= venv/bin/python
PORT ?= 8765
export DEBUG ?= 1

00: $(PYTHON)
	PORT=$(PORT) $(PYTHON) $(shell echo $@*.py)
x00: $(PYTHON)
	PORT=$(PORT) $(PYTHON) $(shell echo $@*.py)

01: $(PYTHON)
	PORT=$(PORT) $(PYTHON) $(shell echo $@*.py)

02: $(PYTHON)
	PORT=$(PORT) $(PYTHON) $(shell echo $@*.py)

03: $(PYTHON)
	PORT=$(PORT) $(PYTHON) $(shell echo $@*.py)

04: $(PYTHON)
	PORT=$(PORT) $(PYTHON) $(shell echo $@*.py)

05: $(PYTHON)
	PORT=$(PORT) $(PYTHON) $(shell echo $@*.py)

########################################
10: $(PYTHON)
	PORT=$(PORT) $(PYTHON) $(shell echo $@*.py)


$(PYTHON):
	python -m venv venv
	$(PYTHON) -m pip install -U -r requirements.txt

client:
	websocat ws://localhost:$(PORT)
