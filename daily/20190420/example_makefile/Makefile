00:
	python parse.py 00*.mk | tee 00.output
01:
	python parse.py 01*.mk | tee 01.output

diff: TARGET ?= 00sample.mk
diff:
	cat ${TARGET} | tee /tmp/before
	python parse.py ${TARGET} | tee /tmp/after
	diff -u /tmp/before /tmp/after | tee /tmp/a.diff
