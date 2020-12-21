SUFFIX ?= $(shell python-config --extension-suffix)

N ?= 100000000000000
run: build
	time python main.py $(N)

build: prime$(SUFFIX)

prime$(SUFFIX):
	clang++ -O2 -Wall -shared -std=c++11 -fPIC -Wl,-undefined,dynamic_lookup `python -m pybind11 --includes` prime.cpp -o prime$(SUFFIX)
