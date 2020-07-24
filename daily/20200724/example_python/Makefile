default: 00 01 02 03 04 05 06 07 08

export DEBUG := 1

PY_FILE = $(shell echo $@*.py)
TEE = | tee
# TEE = \# | tee

00:
	python $(PY_FILE) $(TEE) shell $(PY_FILE).output

# nested class
01:
	python $(PY_FILE) $(TEE) $(PY_FILE).output

# ignore
02:
	python $(PY_FILE) $(TEE) $(PY_FILE).output

# skipped
03:
	python $(PY_FILE) $(TEE) $(PY_FILE).output

# relation
04:
	python $(PY_FILE) $(TEE) $(PY_FILE).output

# relation use directly
05:
	python $(PY_FILE) $(TEE) $(PY_FILE).output

# relation with module
06:
	python $(PY_FILE) $(TEE) $(PY_FILE).output
# relation with module indirectly
07:
	python $(PY_FILE) $(TEE) $(PY_FILE).output
08:
	python $(PY_FILE) $(TEE) $(PY_FILE).output

