PYTHON ?= LOGLEVEL=DEBUG PYTHONASYNCIODEBUG=1 python -W default

00:
	time ${PYTHON} 00*.py
01:
	python 00*.py | ${PYTHON} 01*.py --command 'echo "-" && grep -oP "tag:\S+" | sort | uniq -c | sort -gr'
run:
	python 00*.py | evry -s 1 -- wc
run2:
	python 00*.py | evry -s 1 -c 'echo "-" && grep -oP "tag:\S+" | sort | uniq -c | sort -gr'

setup:
	go get -v github.com/k1LoW/evry
