00:
	python hello.py
01:
	python hello.py -h
02:
	python hello.py --logging=INFO
03:
	DEBUG=1 python hello.py

04: LEVEL ?= DEBUG
04: FORMAT ?= {"level": "%(levelname)s", "funcname": "%(funcName)s", "message": "%(message)r"}
04: STREAM ?= stdout
04:
	LOGGIG_FORMAT=${STREAM} LOGGING_FORMAT='${FORMAT}' LOGGING_LEVEL=${LEVEL} python hello.py

05:
	DEBUG=1 python -m handofcats ./messages.py:debug
