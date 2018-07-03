PREFIX ?= test
SIZE ?=
CMD := python fetch.py -c config.json $(addprefix --size=,${SIZE})

default:
	mkdir -p dist
	${CMD} --input ./${PREFIX}-input.csv | tee dist/${PREFIX}-output.csv

one:
	${CMD} -x 1 -y 2

many:
	${CMD} -x 1 -x 2 -x 3 -x 4 -x 5 -y 2 -y 1 -y 0 -y 1 -y 2
