00:
	$(MAKE) -C $(shell echo $@*/)
01: sqlparser.py
	python $(shell echo $@*.py)
02:
	python $(shell echo $@*.py)
03:
	python $(shell echo $@*.py) | tee $@.output

sqlparser.py:
	python gen.py > $@
