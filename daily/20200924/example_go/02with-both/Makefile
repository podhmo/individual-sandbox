TARGETS := $(shell cat Makefile | grep '^a\.out' | cut -d : -f 1)
default: $(TARGETS)

a.out:
	go build -o $@
a.out.lambda.list:
	go build -o $@ --tags lambda,list
a.out.lambda.add:
	go build -o $@ --tags lambda,add

clean:
	rm -f a.out*
.PHONY: clean
