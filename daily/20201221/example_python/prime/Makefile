N ?= 100000000000000
run: build
	time python main.py $(N)

build: prime.c

prime.c: prime.pyx
	python setup.py build_ext --inplace
