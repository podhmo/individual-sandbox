SHELL := $(shell which bash)
PY_FILE = $(shell echo $@*.py)
EXT ?= svg
# export DEBUG := 1

00:
	dot -T$(EXT) <(python $(PY_FILE)) -o $(PY_FILE).$(EXT)
01:
	dot -T$(EXT) <(python $(PY_FILE)) -o $(PY_FILE).$(EXT)
02:
	dot -T$(EXT) <(python $(PY_FILE)) -o $(PY_FILE).$(EXT)
03:
	dot -T$(EXT) <(python $(PY_FILE)) -o $(PY_FILE).$(EXT)
04:
	dot -T$(EXT) <(python $(PY_FILE)) -o $(PY_FILE).$(EXT)
05:
	dot -T$(EXT) <(python emit.py $(PY_FILE)) -o $(PY_FILE).$(EXT)
06:
	dot -T$(EXT) <(python emit.py $(PY_FILE)) -o $(PY_FILE).$(EXT)
07:
	dot -T$(EXT) <(python emit.py $(PY_FILE)) -o $(PY_FILE).$(EXT)
08:
	dot -Kfdp -T$(EXT) <(python emit.py $(PY_FILE)) -o $(PY_FILE).$(EXT)
09:
	dot -T$(EXT) <(python emit.py $(PY_FILE)) -o $(PY_FILE).$(EXT)
