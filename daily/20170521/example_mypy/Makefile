# OPTS = -v --follow-imports silent
OPTS = --follow-imports silent --strict --show-traceback
TARGET ?= 00

checked-run:
	mypy ${OPTS} ${TARGET}*.py
	python ${TARGET}*.py

00:
	TARGET=00 make checked-run
01:
	TARGET=01 make checked-run
02:
	TARGET=02 make checked-run
03:
	TARGET=03 make checked-run
04:
	TARGET=04 make checked-run
05:
	TARGET=05 make checked-run
