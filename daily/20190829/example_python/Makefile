L ?= 1 2 5 10

00:
	python $@*.py ${L}

01:
	python $@*.py ${L}

run: L = 1 2 5 10
run:
	time $(MAKE) -s 00 L="${L}"
	time $(MAKE) -s 01 L="${L}"
