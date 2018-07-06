PREFIX ?= test
SIZE ?=
CMD := python fetch.py -c config.json $(addprefix --size=,${SIZE})

default:
	mkdir -p dist
	${CMD} --input ./${PREFIX}-input.csv | tee dist/${PREFIX}-output.csv

one:
	${CMD} -x 1 -y 2

xs := 1 2 3 4 5
ys := -2 -1 0 1 2
many:
	${CMD} $(addprefix -x ,$(xs)) $(addprefix -y ,$(ys))
