00:
	python $(shell echo $@*.py) -h
	python $(shell echo $@*.py) --foo xxx -h
01:
	python $(shell echo $@*.py) --foo xxx
	python $(shell echo $@*.py) -h
	python $(shell echo $@*.py) --foo xxx -h
02:
	# python $(shell echo $@*.py) -h
	python $(shell echo $@*.py) --fn f -h
	python $(shell echo $@*.py) --fn g -h
03:
	# python $(shell echo $@*.py) -h
	python $(shell echo $@*.py) --fn f -h
	python $(shell echo $@*.py) --fn g -h

04:
	python $(shell echo $@*.py) -h
	python $(shell echo $@*.py) f -h
	python $(shell echo $@*.py) g -h
05:
	python $(shell echo $@*.py) -h
	python $(shell echo $@*.py) f -h
	python $(shell echo $@*.py) g -h
diff:
	diff -u 03*.py 05*.py > a.diff
.PHONY: diff
